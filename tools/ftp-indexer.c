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

/* The following script is triggred by a cronjob at ftp.gnupg.org to
 * build the index pages.
 *
--8<---------------cut here---------------start------------->8---
#!/bin/sh

set -e
top=/home/ftp
scratch=/home/ftp/.scratch
cd "$top"

opt_force=no
if [ "$1" = "--force" ]; then
  shift
  opt_force=yes
fi

INDEXER=/usr/local/bin/ftp-indexer
if [ ! -x $INDEXER ]; then
  echo "mk-ftp-index.html.sh: Index tool $INDEXER not found - aborting" >&2
  exit 1
fi
INDEXER_OPTS="--reverse-ver --gpgweb --readme --index $scratch/ftp-index.new"
INDEXER_OPTS="$INDEXER_OPTS --exclude README --exclude index.html"


(find . -type d ! -name '\.*' ! -name dev ; echo .) |\
 while read dir rest; do
  dir=${dir##./}
  if cd "$dir"; then
    if [ "$dir" = "." ]; then
      desc="/"
      extraopt="--exclude dev"
    else
      desc="$dir/"
      extraopt=""
    fi

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
         mv $scratch/ftp-index.new .ftp-index
      fi
      rm $scratch/index.html
      [ -f $scratch/index.html.new ] && rm $scratch/index.html.new
      [ -f $scratch/ftp-index.new ] && rm $scratch/ftp-index.new
    else
      mv $scratch/index.html.new index.html
      mv $scratch/ftp-index.new .ftp-index
    fi
  fi
  cd "$top"
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


#define PGMNAME "ftp-indexer"
#define VERSION "0.1"

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


#define digitp(a) ((a) >= '0' && (a) <= '9')
#define VALID_URI_CHARS "abcdefghijklmnopqrstuvwxyz"   \
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"   \
                        "01234567890@"                 \
                        "!\"#$%&'()*+,-./:;<=>?[\\]^_{|}~"


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
static strlist_t opt_exclude;

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
    return -1;  /* Put invalid strings at the end.  */
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
      /* We may only compare up to the next dot and the switch back to
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
             "<title>ftp.gnupg.org:%s</title>\n",
             esc_title);
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
             "<h2>ftp.gnupg.org:%s</h2>\n"
             "<div class=\"outline-text-2\" id=\"text-1\">\n",
             esc_title);

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
             "<li><a href=\"/privacy-policy.html\">Privacy&nbsp;Policy</a>"
             "</li>\n"
             "<li><a href=\"/imprint.html\">Imprint</a>"
             "</li>\n"
             "<li><a href=\"/blog/index.html\">Blog</a>"
             "</li>\n"
             "<li><a href=\"/index.html\">Web</a>"
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
             "<a rel=\"license\""
             " href=\"https://creativecommons.org/licenses/by-sa/4.0/\">"
             "<img alt=\"CC BY-SA 4.0\" style=\"border: 0\""
             " src=\"/share/cc-by-sa_80x15.png\"></a>&nbsp;"
             "This web page is Copyright 2017 GnuPG e.V. and"
             " licensed under a <a rel=\"license\""
             " href=\"https://creativecommons.org/licenses/by-sa/4.0/\">"
             "Creative Commons Attribution-ShareAlike 4.0 International"
             " License</a>.  See <a href=\"https://gnupg.org/copying.html\">"
             "copying</a> for details.\n", stdout);
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


/* Print COUNT files from the array SORTED. */
static void
print_files (finfo_t *sorted, int count)
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
      else if (opt_html)
        printf ("<tr><td width=\"50%%\"><a href=\"%s\">%s</a></td>"
                "<td align=\"right\">%s</td><td align=\"right\">%s</td></tr>\n",
                html_escape_href (fi->name), html_escape (fi->name),
                format_time (fi->mtime), format_size (fi->size));
      else
        printf ("F %s\n", fi->name);
    }

  if (any && opt_gpgweb)
    {
      fputs ("</table>\n"
             "</div>\n\n", stdout);
    }
}


/* Scan DIRECTORY and print an index.
 * FIXME: This does a chdir and does not preserve the old PWD.
 *        The fix is to build the full filename beofre stat'ing.
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
    }

  print_header (title);
  if (opt_files_first)
    {
      print_files (sorted, count);
      print_dirs (sorted, count, at_root);
    }
  else
    {
      print_dirs (sorted, count, at_root);
      print_files (sorted, count);
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
          fprintf (indexfp, "%s:%c:%llu:%lu:\n",
                   percent_escape (fi->name),
                   fi->is_dir? 'd':
                   fi->is_reg? 'r': '?',
                   fi->size,
                   (unsigned long)fi->mtime);
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
                 "  --index FILE    create index FILE\n"
                 "  --exclude NAME  ignore file NAME\n"
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
      else if (!strncmp (*argv, "--", 2))
        die ("unknown option '%s' (use --help)\n", *argv);
    }

  if (argc < 1 || argc > 2)
    die ("usage: " PGMNAME " [options] directory [title]\n");


  scan_directory (argv[0], argv[1]? argv[1]:argv[0]);


  return 0;
}

/*
Local Variables:
compile-command: "cc -Wall -g -o ftp-indexer ftp-indexer.c"
End:
*/
