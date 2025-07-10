/* ftp-indexer.c - Create an HTML index file for an FTP directory
 * Copyright (C) 2017  g10 Code GmbH
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 */

/* The following script is triggered by a cronjob at ftp.gnupg.org to
 * or elsewhere to build the index pages.  example on how to call
 * the script: /etc/mk-ftp-index.html.sh /home/web/foo  foo.gnupg.org
 *
--8<---------------cut here---------------start------------->8---
#!/bin/sh

# The script is a bit complicate because we need to make sure that
# the directory entries do not change while processing them.  A changed
# dir entry would lead to a different index.html file and thus they will
# always be re-created.

PGM=mk-ftp-index.html.sh

set -e

opt_force=no
if [ "$1" = "--force" ]; then
  shift
  opt_force=yes
fi


top="$1"
domain="$2"
if [ ! -d "$top" ]; then
   echo >&2 "usage: $PGM <topdir> [<domain>]"
   exit 1
fi
[ -z "$domain" ] && domain="ftp.gnupg.org"


scratch="$top"/.scratch
cd "$top"


INDEXER=/usr/local/bin/ftp-indexer
if [ ! -x $INDEXER ]; then
  echo >&2 "$PGM: Index tool $INDEXER not found - aborting"
  exit 1
fi
INDEXER_OPTS="--reverse-ver --gpgweb --readme --index $scratch/ftp-index.new"
INDEXER_OPTS="$INDEXER_OPTS --exclude README --exclude index.html --domain $domamain"
INDEXER_OPTS="$INDEXER_OPTS --exclude share"


(find . -type d ! -name '\.*' ! -name dev ; echo .) |\
 while read dir rest; do
  dir=${dir##./}
  cd "$top"
  if cd "$dir"; then
    if [ "$dir" = "." ]; then
      desc="/"
      extraopt="--exclude dev"
    else
      desc="$dir/"
      extraopt=""
    fi

    [ -f .keepindex ] && continue

    [ -f $scratch/index.html ] && rm $scratch/index.html
    [ -f index.html ] && cat index.html >$scratch/index.html
    $INDEXER $INDEXER_OPTS $extraopt . "$desc" >$scratch/index.html.new
    if [ $opt_force = no -a -f $scratch/index.html ]; then
      grep -v '^<meta name="date"' $scratch/index.html \
          | grep -v '^Page last updated on ' >$scratch/index.html.x
      grep -v '^<meta name="date"' $scratch/index.html.new \
          | grep -v '^Page last updated on ' >$scratch/index.html.new.x
      if ! cmp -s $scratch/index.html.x $scratch/index.html.new.x ; then
         mv $scratch/index.html.new index.html
	 chmod o+r index.html
         mv $scratch/ftp-index.new .ftp-index
      fi
      rm $scratch/index.html
      [ -f $scratch/index.html.new ] && rm $scratch/index.html.new
      [ -f $scratch/ftp-index.new ] && rm $scratch/ftp-index.new
    else
      mv $scratch/index.html.new index.html
      chmod o+r index.html
      mv $scratch/ftp-index.new .ftp-index
    fi
  fi
done
--8<---------------cut here---------------end--------------->8---
 *
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <time.h>
#include <errno.h>
#include <assert.h>

#define PGMNAME "ftp-indexer"
#define VERSION "0.2"

#define DIM(v)		     (sizeof(v)/sizeof((v)[0]))
#define DIMof(type,member)   DIM(((type *)0)->member)
#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 5)
#define ATTR_PRINTF(a,b)    __attribute__ ((format (printf,a,b)))
#define ATTR_NR_PRINTF(a,b) __attribute__ ((noreturn,format (printf,a,b)))
#else
#define ATTR_PRINTF(a,b)
#define ATTR_NR_PRINTF(a,b)
#endif
#if __GNUC__ >= 4
# define ATTR_SENTINEL(a) __attribute__ ((sentinel(a)))
#else
# define ATTR_SENTINEL(a)
#endif

#define DIGEST_LENGTH 16  /* We MD5 for thumbnails.  */
/* Figure out a 32 bit unsigned integer type.  */
#if (defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L)
# include <stdint.h>
typedef uint32_t u32;
#else  /* !ISO C-99 */
typedef unsigned int u32;  /* Best guess for old systems.  */
#endif /* !ISO C-99 */



#define digitp(a) ((a) >= '0' && (a) <= '9')
#define VALID_URI_CHARS "abcdefghijklmnopqrstuvwxyz"   \
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"   \
                        "01234567890@"                 \
                        "!\"#$%&'()*+,-./:;<=>?[\\]^_{|}~"

/* For thumbnails we need to compute the MD5.  */
typedef struct
{
  u32 A,B,C,D;
  u32  nblocks;
  unsigned char buf[64];
  int  count;
} DIGEST_CONTEXT;


/* A simple object to keep strings in a list.  */
struct strlist_s
{
  struct strlist_s *next;
  char d[1];
};
typedef struct strlist_s *strlist_t;


/* An object to collect information about files.  */
struct finfo_s
{
  struct finfo_s *next;
  unsigned int is_dir:1;
  unsigned int is_reg:1;
  unsigned int is_image:1;
  time_t mtime;
  unsigned long long size;
  char name[1];
};
typedef struct finfo_s *finfo_t;


static int opt_verbose;
static int opt_debug;
static int opt_reverse;
static int opt_reverse_ver;
static int opt_files_first;
static int opt_html;
static const char *opt_index;
static int opt_gpgweb;
static int opt_readme;
static const char *opt_thumb;
static strlist_t opt_exclude;
static const char *opt_domain;

/* Set to true if this is a big endian host.  Unfortunately there is
   no portable macro to test for it.  Thus we do a runtime test. */
static int big_endian_host;

static void die (const char *format, ...) ATTR_NR_PRINTF(1,2);
static void err (const char *format, ...) ATTR_PRINTF(1,2);
static void inf (const char *format, ...) ATTR_PRINTF(1,2);
static char *xstrconcat (const char *s1, ...) ATTR_SENTINEL(0);


static void
die (const char *fmt, ...)
{
  va_list arg_ptr;

  va_start (arg_ptr, fmt);
  fputs (PGMNAME": ", stderr);
  vfprintf (stderr, fmt, arg_ptr);
  va_end (arg_ptr);
  exit (1);
}


static void
err (const char *fmt, ...)
{
  va_list arg_ptr;

  va_start (arg_ptr, fmt);
  fputs (PGMNAME": ", stderr);
  vfprintf (stderr, fmt, arg_ptr);
  va_end (arg_ptr);
}


static void
inf (const char *fmt, ...)
{
  va_list arg_ptr;

  if (!opt_verbose)
    return;

  va_start (arg_ptr, fmt);
  fputs (PGMNAME": ", stderr);
  vfprintf (stderr, fmt, arg_ptr);
  va_end (arg_ptr);
}


static void *
xmalloc (size_t n)
{
  void *p = malloc (n);
  if (!p)
    die ("out of core\n");
  return p;
}


static void *
xcalloc (size_t n, size_t k)
{
  void *p = calloc (n, k);
  if (!p)
    die ("out of core\n");
  return p;
}


static char *
xstrdup (const char *string)
{
  char *buf = xmalloc (strlen (string) + 1);
  strcpy (buf, string);
  return buf;
}


static inline char *
my_stpcpy (char *a, const char *b)
{
  while (*b)
    *a++ = *b++;
  *a = 0;

  return (char*)a;
}


/* Helper for xstrconcat.  */
static char *
do_strconcat (const char *s1, va_list arg_ptr)
{
  const char *argv[48];
  size_t argc;
  size_t needed;
  char *buffer, *p;

  argc = 0;
  argv[argc++] = s1;
  needed = strlen (s1);
  while (((argv[argc] = va_arg (arg_ptr, const char *))))
    {
      needed += strlen (argv[argc]);
      if (argc >= DIM (argv)-1)
        die ("internal error: too may args for strconcat\n");
      argc++;
    }
  needed++;
  buffer = xmalloc (needed);
  for (p = buffer, argc=0; argv[argc]; argc++)
    p = my_stpcpy (p, argv[argc]);

  return buffer;
}


/* Concatenate the string S1 with all the following strings up to a
   NULL.  Returns a malloced buffer with the new string or NULL on a
   malloc error or if too many arguments are given.  */
static char *
xstrconcat (const char *s1, ...)
{
  va_list arg_ptr;
  char *result;

  if (!s1)
    result = xstrdup ("");
  else
    {
      va_start (arg_ptr, s1);
      result = do_strconcat (s1, arg_ptr);
      va_end (arg_ptr);
    }
  return result;
}


/*
 * Rotate a 32 bit integer by n bytes
 */
#if defined(__GNUC__) && defined(__i386__)
static inline u32
rol( u32 x, int n)
{
	__asm__("roll %%cl,%0"
		:"=r" (x)
		:"0" (x),"c" (n));
	return x;
}
#else
#define rol(x,n) ( ((x) << (n)) | ((x) >> (32-(n))) )
#endif

static void
digest_init (DIGEST_CONTEXT *hd)
{
  hd->A = 0x67452301;
  hd->B = 0xefcdab89;
  hd->C = 0x98badcfe;
  hd->D = 0x10325476;
  hd->nblocks = 0;
  hd->count = 0;
}


/* These are the four functions used in the four steps of the MD5
 * algorithm and defined in the RFC 1321.  The first function is a
 * little bit optimized (as found in Colin Plumbs public domain
 * implementation).  */
#define FF(b, c, d) (d ^ (b & (c ^ d)))
#define FG(b, c, d) FF (d, b, c)
#define FH(b, c, d) (b ^ c ^ d)
#define FI(b, c, d) (c ^ (b | ~d))
static void
transform (DIGEST_CONTEXT *hd, unsigned char *data )
{
  u32 correct_words[16];
  u32 A = hd->A;
  u32 B = hd->B;
  u32 C = hd->C;
  u32 D = hd->D;
  u32 *cwp = correct_words;

  if (big_endian_host)
    {
      int i;
      unsigned char *p2, *p1;
      for(i=0, p1=data, p2=(unsigned char*)correct_words;
          i < 16; i++, p2 += 4 )
        {
          p2[3] = *p1++;
          p2[2] = *p1++;
          p2[1] = *p1++;
          p2[0] = *p1++;
        }
    }
  else
    memcpy (correct_words, data, 64);

#define OP(a, b, c, d, s, T) \
  do			         	   \
    {					   \
      a += FF (b, c, d) + (*cwp++) + T;    \
      a = rol(a, s);			   \
      a += b;				   \
    }					   \
  while (0)

  /* Before we start, one word about the strange constants.
   * They are defined in RFC 1321 as
   *
   *  T[i] = (int) (4294967296.0 * fabs (sin (i))), i=1..64
   */

  /* Round 1.  */
  OP (A, B, C, D,  7, 0xd76aa478);
  OP (D, A, B, C, 12, 0xe8c7b756);
  OP (C, D, A, B, 17, 0x242070db);
  OP (B, C, D, A, 22, 0xc1bdceee);
  OP (A, B, C, D,  7, 0xf57c0faf);
  OP (D, A, B, C, 12, 0x4787c62a);
  OP (C, D, A, B, 17, 0xa8304613);
  OP (B, C, D, A, 22, 0xfd469501);
  OP (A, B, C, D,  7, 0x698098d8);
  OP (D, A, B, C, 12, 0x8b44f7af);
  OP (C, D, A, B, 17, 0xffff5bb1);
  OP (B, C, D, A, 22, 0x895cd7be);
  OP (A, B, C, D,  7, 0x6b901122);
  OP (D, A, B, C, 12, 0xfd987193);
  OP (C, D, A, B, 17, 0xa679438e);
  OP (B, C, D, A, 22, 0x49b40821);

#undef OP
#define OP(f, a, b, c, d, k, s, T)  \
    do								      \
      { 							      \
	a += f (b, c, d) + correct_words[k] + T;		      \
	a = rol(a, s);						      \
	a += b; 						      \
      } 							      \
    while (0)

  /* Round 2.  */
  OP (FG, A, B, C, D,  1,  5, 0xf61e2562);
  OP (FG, D, A, B, C,  6,  9, 0xc040b340);
  OP (FG, C, D, A, B, 11, 14, 0x265e5a51);
  OP (FG, B, C, D, A,  0, 20, 0xe9b6c7aa);
  OP (FG, A, B, C, D,  5,  5, 0xd62f105d);
  OP (FG, D, A, B, C, 10,  9, 0x02441453);
  OP (FG, C, D, A, B, 15, 14, 0xd8a1e681);
  OP (FG, B, C, D, A,  4, 20, 0xe7d3fbc8);
  OP (FG, A, B, C, D,  9,  5, 0x21e1cde6);
  OP (FG, D, A, B, C, 14,  9, 0xc33707d6);
  OP (FG, C, D, A, B,  3, 14, 0xf4d50d87);
  OP (FG, B, C, D, A,  8, 20, 0x455a14ed);
  OP (FG, A, B, C, D, 13,  5, 0xa9e3e905);
  OP (FG, D, A, B, C,  2,  9, 0xfcefa3f8);
  OP (FG, C, D, A, B,  7, 14, 0x676f02d9);
  OP (FG, B, C, D, A, 12, 20, 0x8d2a4c8a);

  /* Round 3.  */
  OP (FH, A, B, C, D,  5,  4, 0xfffa3942);
  OP (FH, D, A, B, C,  8, 11, 0x8771f681);
  OP (FH, C, D, A, B, 11, 16, 0x6d9d6122);
  OP (FH, B, C, D, A, 14, 23, 0xfde5380c);
  OP (FH, A, B, C, D,  1,  4, 0xa4beea44);
  OP (FH, D, A, B, C,  4, 11, 0x4bdecfa9);
  OP (FH, C, D, A, B,  7, 16, 0xf6bb4b60);
  OP (FH, B, C, D, A, 10, 23, 0xbebfbc70);
  OP (FH, A, B, C, D, 13,  4, 0x289b7ec6);
  OP (FH, D, A, B, C,  0, 11, 0xeaa127fa);
  OP (FH, C, D, A, B,  3, 16, 0xd4ef3085);
  OP (FH, B, C, D, A,  6, 23, 0x04881d05);
  OP (FH, A, B, C, D,  9,  4, 0xd9d4d039);
  OP (FH, D, A, B, C, 12, 11, 0xe6db99e5);
  OP (FH, C, D, A, B, 15, 16, 0x1fa27cf8);
  OP (FH, B, C, D, A,  2, 23, 0xc4ac5665);

  /* Round 4.  */
  OP (FI, A, B, C, D,  0,  6, 0xf4292244);
  OP (FI, D, A, B, C,  7, 10, 0x432aff97);
  OP (FI, C, D, A, B, 14, 15, 0xab9423a7);
  OP (FI, B, C, D, A,  5, 21, 0xfc93a039);
  OP (FI, A, B, C, D, 12,  6, 0x655b59c3);
  OP (FI, D, A, B, C,  3, 10, 0x8f0ccc92);
  OP (FI, C, D, A, B, 10, 15, 0xffeff47d);
  OP (FI, B, C, D, A,  1, 21, 0x85845dd1);
  OP (FI, A, B, C, D,  8,  6, 0x6fa87e4f);
  OP (FI, D, A, B, C, 15, 10, 0xfe2ce6e0);
  OP (FI, C, D, A, B,  6, 15, 0xa3014314);
  OP (FI, B, C, D, A, 13, 21, 0x4e0811a1);
  OP (FI, A, B, C, D,  4,  6, 0xf7537e82);
  OP (FI, D, A, B, C, 11, 10, 0xbd3af235);
  OP (FI, C, D, A, B,  2, 15, 0x2ad7d2bb);
  OP (FI, B, C, D, A,  9, 21, 0xeb86d391);

  /* Put checksum in context given as argument.  */
  hd->A += A;
  hd->B += B;
  hd->C += C;
  hd->D += D;
}


/* Update the message digest with the contents of (DATA,DATALEN).  */
static void
digest_write (DIGEST_CONTEXT *hd, const void *data, size_t datalen)
{
  unsigned char *inbuf = (unsigned char *)data;

  if (hd->count == 64) /* Flush the buffer.  */
    {
      transform ( hd, hd->buf );
      hd->count = 0;
      hd->nblocks++;
    }
  if ( !inbuf )
    return;
  if ( hd->count )
    {
      for ( ; datalen && hd->count < 64; datalen-- )
        hd->buf[hd->count++] = *inbuf++;
      digest_write( hd, NULL, 0 );
      if ( !datalen )
        return;
    }

  while( datalen >= 64 )
    {
      transform( hd, inbuf );
      hd->count = 0;
      hd->nblocks++;
      datalen -= 64;
      inbuf += 64;
    }
  for( ; datalen && hd->count < 64; datalen-- )
    hd->buf[hd->count++] = *inbuf++;
}


/* The routine final terminates the computation and returns the
 * digest.  The handle is prepared for a new cycle, but adding bytes
 * to the handle will the destroy the returned buffer.
 * Returns: DIGEST_LENGTH bytes representing the digest.  */
static void
digest_final (DIGEST_CONTEXT *hd)
{
  u32 t, msb, lsb;
  unsigned char *p;

  digest_write(hd, NULL, 0); /* Flush */;

  t = hd->nblocks;
  /* Multiply by 64 to make a byte count.  */
  lsb = t << 6;
  msb = t >> 26;
  /* Add the count.  */
  t = lsb;
  if ( (lsb += hd->count) < t )
    msb++;
  /* Multiply by 8 to make a bit count. */
  t = lsb;
  lsb <<= 3;
  msb <<= 3;
  msb |= t >> 29;

  if ( hd->count < 56 ) /* Enough room is available. */
    {
      hd->buf[hd->count++] = 0x80; /* pad */
      while( hd->count < 56 )
        hd->buf[hd->count++] = 0;  /* pad */
    }
  else /* Need one extra block.  */
    {
      hd->buf[hd->count++] = 0x80; /* pad character */
      while ( hd->count < 64 )
        hd->buf[hd->count++] = 0;
      digest_write (hd, NULL, 0);  /* Flush */;
      memset(hd->buf, 0, 56 );     /* Fill next block with zeroes.  */
    }
  /* Append the 64 bit count. */
  hd->buf[56] = lsb;
  hd->buf[57] = lsb >>  8;
  hd->buf[58] = lsb >> 16;
  hd->buf[59] = lsb >> 24;
  hd->buf[60] = msb;
  hd->buf[61] = msb >>  8;
  hd->buf[62] = msb >> 16;
  hd->buf[63] = msb >> 24;

  transform( hd, hd->buf );
  p = hd->buf;
#define X(a) do { *p++ = hd->a      ; *p++ = hd->a >> 8;      \
	          *p++ = hd->a >> 16; *p++ = hd->a >> 24; } while(0)
  X(A);
  X(B);
  X(C);
  X(D);
#undef X
}


/* If SPECIAL is NULL this function escapes in forms mode.  */
static size_t
escape_data (char *buffer, const void *data, size_t datalen,
             const char *special)
{
  int forms = !special;
  const unsigned char *s;
  size_t n = 0;

  if (forms)
    special = "%;?&=";

  for (s = data; datalen; s++, datalen--)
    {
      if (forms && *s == ' ')
        {
	  if (buffer)
	    *buffer++ = '+';
	  n++;
        }
      else if (forms && *s == '\n')
        {
	  if (buffer)
	    memcpy (buffer, "%0D%0A", 6);
	  n += 6;
        }
      else if (forms && *s == '\r' && datalen > 1 && s[1] == '\n')
        {
	  if (buffer)
	    memcpy (buffer, "%0D%0A", 6);
	  n += 6;
          s++;
          datalen--;
        }
      else if (strchr (VALID_URI_CHARS, *s) && !strchr (special, *s))
	{
	  if (buffer)
	    *(unsigned char*)buffer++ = *s;
	  n++;
	}
      else
	{
	  if (buffer)
	    {
	      snprintf (buffer, 4, "%%%02X", *s);
	      buffer += 3;
	    }
	  n += 3;
	}
    }
  return n;
}


static int
insert_escapes (char *buffer, const char *string,
		const char *special)
{
  return escape_data (buffer, string, strlen (string), special);
}


/* Allocate a new string from STRING using standard HTTP escaping as
 * well as escaping of characters given in SPECIALS.  A common pattern
 * for SPECIALS is "%;?&=". However it depends on the needs, for
 * example "+" and "/: often needs to be escaped too.  Returns NULL on
 * failure and sets ERRNO.  If SPECIAL is NULL a dedicated forms
 * encoding mode is used.  */
static char *
http_escape_string (const char *string, const char *specials)
{
  int n;
  char *buf;

  n = insert_escapes (NULL, string, specials);
  buf = xmalloc (n+1);
  insert_escapes (buf, string, specials);
  buf[n] = 0;
  return buf;
}


/* Same as http_escape_string but with an explict length.  */
static char *
http_escape_buffer (const char *string, size_t length, const char *specials)
{
  int n;
  char *buf;

  n = escape_data (NULL, string, length, specials);
  buf = xmalloc (n+1);
  escape_data (buf, string, length, specials);
  buf[n] = 0;
  return buf;
}


/* Percent-escape the string STR by replacing colons with '%3a'.   */
static char *
do_percent_escape (const char *str)
{
  int i, j;
  char *ptr;

  for (i=j=0; str[i]; i++)
    if (str[i] == ':' || str[i] == '%' || str[i] == '\n')
      j++;
  ptr = xmalloc (i + 2 * j + 1);
  i = 0;
  while (*str)
    {
      if (*str == ':')
	{
	  ptr[i++] = '%';
	  ptr[i++] = '3';
	  ptr[i++] = 'a';
	}
      else if (*str == '%')
	{
	  ptr[i++] = '%';
	  ptr[i++] = '2';
	  ptr[i++] = '5';
	}
      else if (*str == '\n')
	{
	  /* The newline is problematic in a line-based format.  */
	  ptr[i++] = '%';
	  ptr[i++] = '0';
	  ptr[i++] = 'a';
	}
      else
	ptr[i++] = *str;
      str++;
    }
  ptr[i] = '\0';

  return ptr;
}


/* Simple percent escape for colon based listings.  Returns a
 * statically allocated buffer.  */
static char *
percent_escape (const char *str)
{
  static char *buffer;

  free (buffer);
  buffer = do_percent_escape (str);
  return buffer;
}


/* Escape STRING at a max length of N for use in HTML.  Returns a
 * statically allocated buffer.  */
static const char *
html_escape_n (const char *string, size_t length)
{
  static char *buffer;
  char *p;

  /* The escaped string may be up to 6 times of STRING due to the
     expansion of '\"' to "&quot;".  */
  free (buffer);
  p = buffer = xmalloc (6 * length + 1);
  for (; *string && length; string++, length--)
    {
      switch (*string)
        {
        case '\"': p = my_stpcpy (p, "&quot;"); break;
        case '&':  p = my_stpcpy (p, "&amp;"); break;
        case '<':  p = my_stpcpy (p, "&lt;"); break;
        case '>':  p = my_stpcpy (p, "&gt;"); break;
        default:   *p++ = *string; break;
        }
    }
  *p = 0;
  return buffer;
}


/* Escape STRING for use in HTML.  Returns a statically allocated
 * buffer.  noet that this buffer is shared with the buffer returned
 * by html_escape_n.  */
static const char *
html_escape (const char *string)
{
  return html_escape_n (string, strlen (string));
}


/* Escape STRING but insert <a> for one https link.  */
static const char *
html_escape_detect_link (const char *string)
{
  const char *start, *s;
  char *part1, *url, *part2, *part3;
  size_t urllen;
  char *buffer, *p;

  start = strstr (string, "https://");
  if (!start || !start[8] || start[8] == ' ' || start[8] == '\t')
    return html_escape (string);
  if (!(start == string || start[-1] == ' ' || start[-1] == '\t'
        || start[-1] == '<'))
    return html_escape (string);

  urllen = 0;
  for (s = start; *s && *s != ' ' && *s != '\t' && *s != '>'; s++)
    urllen++;

  part1 = xstrdup (html_escape_n (string, start-string));
  url = http_escape_buffer (start, urllen, "\"");
  part2 = xstrdup (html_escape_n (start, urllen));
  part3 = xstrdup (html_escape (start + urllen));

  buffer = xmalloc (strlen (part1) + strlen (url)
                    + strlen (part2) + strlen (part3) + 100);
  p = my_stpcpy (buffer, part1);
  p = my_stpcpy (p, "<a href=\"");
  p = my_stpcpy (p, url);
  p = my_stpcpy (p, "\">");
  p = my_stpcpy (p, part2);
  p = my_stpcpy (p, "</a>");
  my_stpcpy (p, part3);

  free (part1);
  free (url);
  free (part2);
  free (part3);
  return buffer;
}



/* Escape STRING for use as a HREF attribute.  Returns a statically
 * allocated buffer.  */
static const char *
html_escape_href (const char *string)
{
  static char *buffer;

  free (buffer);
  buffer = http_escape_string (string, "\"");
  return buffer;
}


/* Format T and return a statically allocated buffer.  */
static const char *
format_time_now (int human)
{
  static char buffer[40];
  struct tm *tp;
  time_t now;

  time (&now);
  tp = gmtime (&now);
  if (!tp)
    *buffer = 0;

  if (human)
    snprintf (buffer, sizeof buffer, "%04d-%02d-%02d",
              1900 + tp->tm_year, tp->tm_mon+1, tp->tm_mday);
  else
    snprintf (buffer, sizeof buffer, "%04d%02d%02dT%02d%02d%02dZ",
              1900 + tp->tm_year, tp->tm_mon+1, tp->tm_mday,
              tp->tm_hour, tp->tm_min, tp->tm_sec);

  return buffer;
}


/* Format T and return a statically allocated buffer.  */
static const char *
format_time (time_t t)
{
  static char buffer[80];
  struct tm *tp;

  tp = gmtime (&t);
  if (!tp)
    *buffer = 0;
  else if (opt_html)
    snprintf (buffer, sizeof buffer,
              "<abbr title=\"%04d-%02d-%02d&nbsp;%02d:%02d:%02d UTC\">"
              "%04d-%02d-%02d</abbr>",
              1900 + tp->tm_year, tp->tm_mon+1, tp->tm_mday,
              tp->tm_hour, tp->tm_min, tp->tm_sec,
              1900 + tp->tm_year, tp->tm_mon+1, tp->tm_mday);
  else
    snprintf (buffer, sizeof buffer, "%04d%02d%02dT%02d%02d%02dZ",
              1900 + tp->tm_year, tp->tm_mon+1, tp->tm_mday,
              tp->tm_hour, tp->tm_min, tp->tm_sec);
  return buffer;
}


/* Format SIZE and return a statically allocated buffer.  */
static const char *
format_size (unsigned long long size)
{
  static char buffer[80];
  const char *suffix;
  unsigned long long val = size;

  if (size < 1024)
    {
      val = size;
      suffix = "";
    }
  else if (size < 1024 * 1024)
    {
      val = size / 1024;
      suffix = "k";
    }
  else if (size < 1024 * 1024 * 1024)
    {
      val = size / (1024 * 1024);
      suffix = "M";
    }
  else
    {
      val = size / (1024 * 1024 * 1024);
      suffix = "G";
    }

  if (opt_html)
    snprintf (buffer, sizeof buffer,
              "<abbr title=\"%llu byte%s\">%llu%s</abbr>",
              size, size == 1? "":"s",
              val, suffix);
  else
    snprintf (buffer, sizeof buffer, "%llu%s", val, suffix);

  return buffer;
}


/* This function parses the first portion of the version number S and
 * stores it at NUMBER.  On success, this function returns a pointer
 * into S starting with the first character, which is not part of the
 * initial number portion; on failure, NULL is returned.  */
static const char*
parse_version_number (const char *s, int *number)
{
  int val = 0;

  if (*s == '0' && digitp (s[1]))
    return NULL;  /* Leading zeros are not allowed.  */
  for (; digitp (*s); s++)
    {
      val *= 10;
      val += *s - '0';
    }
  *number = val;
  return val < 0 ? NULL : s;
}


/* This function breaks up the complete string-representation of the
 * version number S, which is of the following struture: <major
 * number>.<minor number>.<micro number><patch level>.  The major,
 * minor and micro number components will be stored in *MAJOR, *MINOR
 * and *MICRO.
 *
 * On success, the last component, the patch level, will be returned;
 * in failure, NULL will be returned.  */
static const char *
parse_version_string (const char *s, int *major, int *minor, int *micro)
{
  s = parse_version_number (s, major);
  if (!s || *s != '.')
    return NULL;
  s++;
  s = parse_version_number (s, minor);
  if (!s || *s != '.')
    return NULL;
  s++;
  s = parse_version_number (s, micro);
  if (!s)
    return NULL;
  return s; /* patchlevel */
}


/* Return a pointer to the first timestamp digit.  A timestamp is
 * expected to have this format "[12]yyymmddhhmm[ss]".  */
static const char *
is_timestamp (const char *string)
{
  const char *s;
  int i;

  if (!string || !(*string == '1' || *string == '2'))
    return NULL;
  for (i=0,s=string; digitp (*s); s++)
    i++;
  return (i == 12 || i == 14)? string : NULL;
}


/* Compare function for version strings.  */
static int
compare_version_strings (const char *a, const char *b)
{
  int a_major, a_minor, a_micro;
  int b_major, b_minor, b_micro;
  const char *a_plvl, *b_plvl;

  a_plvl = parse_version_string (a, &a_major, &a_minor, &a_micro);
  if (!a_plvl)
    a_major = a_minor = a_micro = 0;

  b_plvl = parse_version_string (b, &b_major, &b_minor, &b_micro);
  if (!b_plvl)
    b_major = b_minor = b_micro = 0;

  if (!a_plvl && !b_plvl)
    {
      /* Put invalid strings at the end.  But first check whether they
       * are both timestamps and use strcmp in this case. */
      if ( (a_plvl = is_timestamp (a)) && (b_plvl = is_timestamp (b)))
        return strcmp (a_plvl, b_plvl);

      return -1;
    }
  if (a_plvl && !b_plvl)
    return 1;
  if (!a_plvl && b_plvl)
    return -1;

  if (a_major > b_major)
    return 1;
  if (a_major < b_major)
    return -1;

  if (a_minor > b_minor)
    return 1;
  if (a_minor < b_minor)
    return -1;

  if (a_micro > b_micro)
    return 1;
  if (a_micro < b_micro)
    return -1;

  if (opt_reverse_ver && !opt_reverse)
    {
      /* We may only compare up to the next dot and then switch back to
       * regular order.  */
      for (; *a_plvl && *b_plvl; a_plvl++, b_plvl++)
        {
          if (*a_plvl == '.' && *b_plvl == '.')
            return 0 - strcmp (a_plvl, b_plvl);
          else if (*a_plvl == '.')
            return 1;  /* B is larger but we need to reverse. */
          else if (*b_plvl == '.')
            return -1; /* A is larger but we need to reverse. */
          else if (*a_plvl != *b_plvl)
            break;
        }
      if (*a_plvl == *b_plvl)
        return 0;
      else
        return (*(signed char *)b_plvl - *(signed char *)a_plvl);
    }
  else
    return strcmp (a_plvl, b_plvl);
}


/* If string looks like a file name with a version nuymber, return a
 * pointer to the version number part; else return NULL.  */
static const char *
find_version_string (const char *s)
{
  do
    {
      s = strchr (s, '-');
      if (!s)
        return NULL; /* Version string must be prefixed with a dash.  */
      s++;
    }
  while (!digitp (*s));

  return s;
}


/* Sort function for the directory listing.  */
static int
sort_finfo (const void *arg_a, const void *arg_b)
{
  const finfo_t *a = arg_a;
  const finfo_t *b = arg_b;
  const char *astr, *bstr;
  const char *aver, *bver;

  if (opt_reverse)
    {
      astr = (*b)->name;
      bstr = (*a)->name;
    }
  else
    {
      astr = (*a)->name;
      bstr = (*b)->name;
    }

  aver = find_version_string (astr);
  bver = aver? find_version_string (bstr) : NULL;

  if (aver && bver
      && (aver - astr) == (bver - bstr)
      && !memcmp (astr, bstr, (aver - astr)))
    {
      if (opt_reverse_ver)
        return 0 - compare_version_strings (aver, bver);
      else
        return compare_version_strings (aver, bver);
    }

  return strcmp(astr, bstr);
}


/* Return a string to a constant buffer with the thumbnail matching
 * FILE.  CWD needs to be the current WD.  */
static const char *
get_thumbnail (const char *cwd, const char *file)
{
  DIGEST_CONTEXT ctx;
  static char thumbname[7 + DIGEST_LENGTH*2 + 4 + 1];
  int i;

  digest_init (&ctx);
  digest_write (&ctx, "file://", 7);
  if (*cwd != '/')
    err ("CWD '%s' is not an absolute dir\n", cwd);
  else
    digest_write (&ctx, cwd, strlen (cwd));
  digest_write (&ctx, "/", 1);
  digest_write (&ctx, file, strlen (file));
  digest_final (&ctx);

  strcpy (thumbname, "normal/");
  for (i=0; i < DIGEST_LENGTH; i++)
    snprintf (thumbname+7+2*i, 10, "%02x", ctx.buf[i]);
  strcat (thumbname, ".png");

  return thumbname;
}


/* Note: This function assumes that the CWD is the listed directory.  */
static void
print_header (const char *title)
{
  const char *esc_title;

  if (!opt_html)
    return;

  esc_title = html_escape (title);

  if (opt_gpgweb)
    {
      FILE *readme;
      char line[256];
      char *p;
      int c;

      fputs ("<!--?xml version=\"1.0\" encoding=\"utf-8\"?-->\n"
             "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
             " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
             "<html xmlns=\"http://www.w3.org/1999/xhtml\""
             " xml:lang=\"en\" lang=\"en\">\n", stdout);
      printf("<head>\n"
             "<title>%s:%s</title>\n",
             opt_domain, esc_title);
      fputs ("<meta http-equiv=\"Content-Type\""
             " content=\"text/html; charset=UTF-8\"/>\n", stdout);
      printf("<meta name=\"date\" content=\"%s\"/>\n", format_time_now (0));
      fputs ("<meta name=\"generator\" content=\""PGMNAME" v"VERSION"\"/>\n"
             "<meta name=\"viewport\""
             " content=\"width=device-width, initial-scale=1\"/>\n"
             "<link rel=\"stylesheet\" href=\"/share/site.css\""
             " type=\"text/css\"/>\n"
             "</head>\n", stdout);

      fputs ("<body>\n"
             "<div id=\"wrapper\">\n"
             "<div id=\"header\"><a href=\"https://gnupg.org/index.html\""
             " class=\"logo\">"
             "<img src=\"/share/logo-gnupg-light-purple-bg.png\"></a>&nbsp;"
             "</div>\n", stdout);

      printf("<main>\n"
             "<div id=\"content\">\n"
             "<h2>%s:%s</h2>\n"
             "<div class=\"outline-text-2\" id=\"text-1\">\n",
             opt_domain, esc_title);

      readme = fopen ("README", "r");
      if (opt_readme && (readme = fopen ("README", "r")))
        {
          fputs ("<pre class=\"ftp-readme\">\n", stdout);
          while (fgets (line, sizeof line, readme))
            {
              int no_lf = 0;
              /* Eat up the rest of an incomplete line.  */
              if (!*line)
                no_lf = 1;
              else if (line[strlen (line)-1] != '\n')
                {
                  no_lf = 1;
                  while ((c = getc (readme)) != EOF && c != '\n')
                    ;
                }

              /* Replace empty lines with a leading doc by an empty
               * line.  These lines are used on FTP servers to avoid
               * problems with broken FTP cleints.  */
              if (*line == '.')
                {
                  for (p=line+1; (*p == ' ' || *p == '\t' || *p == '\n'); p++)
                    ;
                  if (!*p)
                    {
                      putchar ('\n');
                      *line = 0;
                    }
                }

              if (*line)
                fputs (html_escape_detect_link (line), stdout);
              if (no_lf)
                putchar ('\n');
            }
          fputs ("</pre>\n", stdout);
          fclose (readme);
        }
      fputs ("</div>\n", stdout);

   }
  else
    {
      printf ("<html>\n"
              "<head>\n"
              "<title>Index of %s</title>\n"
              "</head>\n"
              "<body bgcolor=\"#ffffff\">\n"
              "<h2>Index of %s</h2>\n"
              "<table class=\"ftp\">\n",
              esc_title, esc_title);
    }
}


static void
print_footer (void)
{
  if (!opt_html)
    return;

  if (opt_gpgweb)
    {
      fputs ("</div><!-- end content -->\n"
             "</main>\n"
             "<div id=\"footer\">\n", stdout);
      fputs ("<div id=\"nav_bottom\">\n"
             "<ul>\n"
             "<li><a href=\"https://gnupg.org/privacy-policy.html\">Privacy&nbsp;Policy</a>"
             "</li>\n"
             "<li><a href=\"https://gnupg.org/imprint.html\">Imprint</a>"
             "</li>\n"
             "<li><a href=\"https://gnupg.org/blog/index.html\">Blog</a>"
             "</li>\n"
             "<li><a href=\"https://gnupg.org/index.html\">Web</a>"
             "</li>\n"
             "</ul>\n"
             "</div>\n", stdout);

      fputs ("<div class=\"footerbox\">\n"
             "<a><img src=\"/share/traueranzeige-g10_v2015.png\""
             " alt=\"Traueranzeige: Wir nehmen Abschied von einem"
             " sicher geglaubten Freund, dem | Fernmeldegeheimniss"
             " | (Artikel 10 Grundgesetz) | * 23. Mai 1949,"
             " + 18. Dezember 2015\" title=\"Article 10 of the"
             " German constitution (communication privacy) is"
             " not anymore with us.\" height=\"73px\" width=\"200px\"></a>\n"
             "<p></p>\n"
             "</div>\n", stdout);

      fputs ("<div id=\"cpyright\">\n"
             "This web page is generated from a file listing"
             " and as such not copyrightable.\n", stdout);
      printf("Page last updated on %s.\n", format_time_now (1));
      fputs ("</div>\n"
             "</div>\n"
             "</div><!-- end wrapper -->\n"
             "</body>\n"
             "</html>\n", stdout);
    }
  else
    {
      printf ("</table>\n"
              "</body>\n"
              "</html>\n");
    }
}


/* Print COUNT directories from the array SORTED.
 * Note: This function assumes that the CWD is the listed directory.  */
static void
print_dirs (finfo_t *sorted, int count, int at_root)
{
  int idx;
  finfo_t fi;
  int any = 0;
  char *title = NULL;

  for (idx=0; idx < count; idx++)
    {
      fi = sorted[idx];
      if (!fi->is_dir)
        continue;

      if (!any && opt_html)
        {
          any = 1;

          if (opt_gpgweb)
            {
              fputs ("<h3>Directories</h3>\n"
                     "<div class=\"outline-text-3\">\n"
                     "<table class=\"ftp\">\n", stdout);

              if (!at_root)
                fputs ("<tr><td><img src=\"/share/up.png\""
                       " width=\"22\" height=\"22\"/></td>"
                       "<td><a href=\"../\">"
                       "Parent Directory</a></td></tr>\n", stdout);
            }
          else
            {
              fputs ("<tr><td>&nbsp</td>"
                     "<td colspan=3><h3>Directories</h3></td></tr>\n",
                     stdout);
              if (!at_root)
                fputs ("<tr><td><a href=\"../\">"
                       "Parent Directory</a></td></tr>\n", stdout);
            }
        }

      free (title);
      title = NULL;
      if (opt_gpgweb)
        {
          char *fname;
          FILE *fp;

          fname = xstrconcat (fi->name, "/", ".title", NULL);
          fp = fopen (fname, "r");
          free (fname);
          if (fp)
            {
              char line[200];

              if (fgets (line, sizeof line, fp) && *line)
                {
                  if (line[strlen(line)-1] == '\n')
                    line[strlen(line)-1] = 0;
                  title = xstrdup (html_escape (line));
                }
              fclose (fp);
            }
        }

      if (opt_html)
        {
          if (opt_gpgweb)
            printf ("<tr><td><img src=\"/share/folder.png\""
                    " width=\"22\" height=\"22\"/></td>"
                    "<td><a href=\"%s\">%s</a></td>",
                    html_escape_href (fi->name), html_escape (fi->name));
          else
            printf ("<tr><td width=\"40%%\"><a href=\"%s\">%s</a></td>",
                    html_escape_href (fi->name), html_escape (fi->name));
          if (title)
            printf ("<td>%s</td>", title);
          fputs ("</tr>\n", stdout);
        }
      else
        printf ("D %s\n", fi->name);
    }

  if (any && opt_gpgweb)
    {
      fputs ("</table>\n"
             "</div>\n\n", stdout);
    }
  else if (opt_gpgweb && !at_root)
    {
      /* !any - need to print an UP link */
      fputs ("<div class=\"outline-text-3\">\n"
             "<table class=\"ftp\">\n"
             "<tr><td><img src=\"/share/up.png\""
             " width=\"22\" height=\"22\"/></td>"
             "<td><a href=\"../\">"
             "Parent Directory</a></td></tr>\n"
             "</table>\n"
             "</div>\n", stdout);


    }

  free (title);
}


/* Print COUNT files from the array SORTED.  CWD is the current working
 * directory. */
static void
print_files (const char *cwd, finfo_t *sorted, int count)
{
  int idx;
  finfo_t fi;
  int any = 0;

  for (idx=0; idx < count; idx++)
    {
      fi = sorted[idx];
      if (!fi->is_reg)
        continue;

      if (!any && opt_html)
        {
          any = 1;
          if (opt_gpgweb)
            {
              fputs ("<h3>Files</h3>\n"
                     "<div class=\"outline-text-3\">\n"
                     "<table class=\"ftp\">\n", stdout);
            }
          else
            fputs ("<tr><td colspan=3><h3>Files</h3></td></tr>\n",
                   stdout);

        }

      if (opt_gpgweb)
        printf ("<tr><td><img src=\"/share/%s.png\""
                " width=\"22\" height=\"22\"/></td>"
                "<td><a href=\"%s\">%s</a></td>"
                "<td align=\"right\">%s</td><td align=\"right\">%s</td></tr>\n",
                strstr (fi->name, ".sig")? "document":
                strstr (fi->name, ".tar")? "tar" : "document",
                html_escape_href (fi->name), html_escape (fi->name),
                format_time (fi->mtime), format_size (fi->size));
      else if (opt_html && opt_thumb  && fi->is_image)
        printf ("<tr><td width=\"50%%\"><a href=\"%s\">"
                "<img src=\"%s/%s\"/>"
                "</a></td>"
                "<td align=\"right\">%s</td><td align=\"right\">%s</td></tr>\n",
                html_escape_href (fi->name),
                opt_thumb,
                get_thumbnail (cwd, fi->name),
                format_time (fi->mtime), format_size (fi->size));
      else if (opt_html)
        printf ("<tr><td width=\"50%%\"><a href=\"%s\">%s</a></td>"
                "<td align=\"right\">%s</td><td align=\"right\">%s</td></tr>\n",
                html_escape_href (fi->name), html_escape (fi->name),
                format_time (fi->mtime), format_size (fi->size));
      else if (opt_thumb && fi->is_image)
        printf ("F %s %s/%s\n",
                fi->name, opt_thumb, get_thumbnail (cwd, fi->name));
      else
        printf ("F %s\n", fi->name);
    }

  if (any && opt_gpgweb)
    {
      fputs ("</table>\n"
             "</div>\n\n", stdout);
    }
}


static int
has_image_suffix (const char *name)
{
  const char *suffixes[] =  { "jpeg", "jpg", "png", "tiff", "gif",
                              "JPEG", "JPG", "PNG", "TIFF", "GIF" };
  unsigned int i, n, namelen;

  namelen = strlen (name);
  for (i=0; i < DIM (suffixes); i++)
    if (namelen > (n=strlen (suffixes[i]) + 1)
        && name[namelen-n] == '.'
        && !strcmp (name + namelen - n + 1,  suffixes[i]))
      return 1;
  return 0;
}


/* SCAN DIRECTORY and print an index.
 * FIXME: This does a chdir and does not preserve the old PWD.
 *        The fix is to build the full filename before stat'ing.
 */
static void
scan_directory (const char *directory, const char *title)
{
  DIR *dir;
  struct dirent *dentry;
  finfo_t fi;
  finfo_t finfo = NULL;
  finfo_t *sorted;
  int count = 0;
  int idx;
  size_t len;
  strlist_t sl;
  int at_root = 0;
  char cwd[4096];

  if (opt_gpgweb)
    {
      if (!strcmp (title, "/"))
        at_root = 1;
    }
  else if (!strcmp (directory, "/"))
    at_root = 1;

  dir = opendir (directory);
  if (!dir)
    {
      err ("can't open directory '%s': %s\n", directory, strerror (errno));
      return;
    }

  while (errno=0,(dentry = readdir (dir)))
    {
      if (*dentry->d_name == '.')
        continue;  /* Skip self, parent, and hidden directories.  */
      len = strlen (dentry->d_name);
      if (!len)
        continue;  /* Empty filenames should actually not exist.  */
      if (dentry->d_name[len-1] == '~')
        continue;  /* Skip backup files.  */
      for (sl = opt_exclude; sl; sl = sl->next)
        if (!strcmp (sl->d, dentry->d_name))
          break;
      if (sl)
        continue; /* Skip excluded names.  */
      fi = xcalloc (1, sizeof *fi + strlen (dentry->d_name));
      strcpy (fi->name, dentry->d_name);
      fi->next = finfo;
      finfo = fi;
      count++;
    }
  if (errno)
    die ("error reading directory '%s': %s\n", directory, strerror (errno));
  closedir (dir);

  sorted = xcalloc (count, sizeof *sorted);
  for (fi=finfo, idx=0; fi; fi = fi->next)
    sorted[idx++] = fi;

  inf ("directory '%s' has %d files\n", directory, count);
  qsort (sorted, count, sizeof *sorted, sort_finfo);

  if (chdir (directory))
    die ("cannot chdir to '%s': %s\n", directory, strerror (errno));

  if (!getcwd (cwd, sizeof cwd -1))
    strcpy (cwd, ".");  /* Ooops.  */

  for (idx=0; idx < count; idx++)
    {
      struct stat sb;

      fi = sorted[idx];
      if (stat (fi->name, &sb))
        {
          err ("cannot stat '%s': %s\n", fi->name, strerror (errno));
          continue;
        }

      fi->is_dir = !!S_ISDIR(sb.st_mode);
      fi->is_reg = !!S_ISREG(sb.st_mode);
      fi->size = fi->is_reg? sb.st_size : 0;
      fi->mtime = sb.st_mtime;
      fi->is_image = !!has_image_suffix (fi->name);
    }

  print_header (title);
  if (opt_files_first)
    {
      print_files (cwd, sorted, count);
      print_dirs (sorted, count, at_root);
    }
  else
    {
      print_dirs (sorted, count, at_root);
      print_files (cwd, sorted, count);
    }
  print_footer ();

  /* We create the index file in the current directory.  */
  if (opt_index)
    {
      FILE *indexfp = fopen (opt_index, "w");
      if (!indexfp)
        die ("error creating '%s' for '%s': %s\n",
             opt_index, directory, strerror (errno));

      for (idx=0; idx < count; idx++)
        {
          fi = sorted[idx];
          fprintf (indexfp, "%s:%c:%llu:%lu:%s:\n",
                   percent_escape (fi->name),
                   fi->is_dir? 'd':
                   fi->is_reg? 'r': '?',
                   fi->size,
                   (unsigned long)fi->mtime,
                   (opt_thumb && fi->is_image)?
                      get_thumbnail (cwd, fi->name) : "");
        }
      if (ferror (indexfp))
        die ("error writing '%s' for '%s': %s\n",
             opt_index, directory, strerror (errno));
      /* Fixme: Check for close errors.  */
      fclose (indexfp);
    }

  free (sorted);
  while ((fi = finfo))
    {
      fi = finfo->next;
      free (finfo);
      finfo = fi;
    }
}


int
main (int argc, char **argv)
{
  int last_argc = -1;
  strlist_t sl;

  assert (sizeof (u32) == 4);
  {
    union { u32 u; char b[4]; } foo;
    foo.u = 32;
    big_endian_host = !foo.b[0];
  }
  opt_domain = "ftp.gnupg.org";

  if (argc < 1)
    die ("Hey, read up on how to use exec(2)\n");
  argv++; argc--;

  while (argc && last_argc != argc )
    {
      last_argc = argc;
      if (!strcmp (*argv, "--"))
        {
          argc--; argv++;
          break;
        }
      else if (!strcmp (*argv, "--version"))
        {
          fputs (PGMNAME " " VERSION "\n"
                 "Copyright (C) 2017 g10 Code GmbH\n"
                 "License GPLv3+: GNU GPL version 3 or later"
                 " <https://gnu.org/licenses/gpl.html>\n"
                 "This is free software: you are free to change"
                 " and redistribute it.\n"
                 "There is NO WARRANTY, to the extent permitted by law.\n",
                 stdout);
          exit (0);
        }
      else if (!strcmp (*argv, "--help"))
        {
          fputs ("usage: " PGMNAME " [options] directory [title]\n"
                 "Print an index for an FTP directory.\n\n"
                 "Options:\n"
                 "  --version       print program version\n"
                 "  --verbose       verbose diagnostics\n"
                 "  --debug         flyswatter\n"
                 "  --reverse       reverse sort order\n"
                 "  --reverse-ver   reverse only the version number order\n"
                 "  --files-first   print files before directories\n"
                 "  --html          output HTML\n"
                 "  --gpgweb        output HTML as used at gnupg.org\n"
                 "  --readme        include README file\n"
                 "  --thumb DIR     include standard thumbnails using DIR\n"
                 "  --index FILE    create index FILE\n"
                 "  --exclude NAME  ignore file NAME\n"
                 "  --domain NAME   use NAME instead of ftp.gnupg.org\n"
                 , stdout);
          exit (0);
        }
      else if (!strcmp (*argv, "--verbose"))
        {
          opt_verbose++;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--debug"))
        {
          opt_debug++;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--reverse"))
        {
          opt_reverse = 1;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--reverse-ver"))
        {
          opt_reverse_ver = 1;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--files-first"))
        {
          opt_files_first = 1;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--readme"))
        {
          opt_readme = 1;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--thumb"))
        {
          argc--; argv++;
          if (!argc || !**argv)
            die ("argument missing for option '%s'\n", argv[-1]);
          opt_thumb = *argv;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--html"))
        {
          opt_html = 1;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--index"))
        {
          argc--; argv++;
          if (!argc || !**argv)
            die ("argument missing for option '%s'\n", argv[-1]);
          opt_index = *argv;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--gpgweb"))
        {
          opt_gpgweb = opt_html = 1;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--exclude"))
        {
          argc--; argv++;
          if (!argc || !**argv)
            die ("argument missing for option '%s'\n", argv[-1]);
          sl = xmalloc (sizeof *sl + strlen (*argv));
          strcpy (sl->d, *argv);
          sl->next = opt_exclude;
          opt_exclude = sl;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--domain"))
        {
          argc--; argv++;
          if (!argc || !**argv)
            die ("argument missing for option '%s'\n", argv[-1]);
          opt_domain = *argv;
          argc--; argv++;
        }
      else if (!strncmp (*argv, "--", 2))
        die ("unknown option '%s' (use --help)\n", *argv);
    }

  if (argc < 1 || argc > 2)
    die ("usage: " PGMNAME " [options] directory [title]\n");

  if (opt_html && opt_thumb)
    {
      char *p = xstrdup (html_escape_href (opt_thumb));
      opt_thumb = p;
    }

  scan_directory (argv[0], argv[1]? argv[1]:argv[0]);


  return 0;
}

/*
Local Variables:
compile-command: "cc -Wall -g -o ftp-indexer ftp-indexer.c"
End:
*/
