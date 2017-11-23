/* txxmpp.c - Transmit a message to an XMPP account
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
 * SPDX-License-Identifier: GPL-3.0+
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <ctype.h>
#include <strophe.h>  /* The low-level xmpp library.  */

#define PGMNAME "txxmpp"
#define VERSION "0.9"

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


static int opt_verbose;
static int opt_debug;
static const char *opt_user;
static const char *opt_pass;
static const char *opt_resource;
static const char *opt_subject;
static int opt_chat;
static char **opt_recipients; /* NULL terminated array with recipients.  */
static char *the_message;

static void die (const char *format, ...) ATTR_NR_PRINTF(1,2);
static void err (const char *format, ...) ATTR_PRINTF(1,2);
static void inf (const char *format, ...) ATTR_PRINTF(1,2);
static void dbg (const char *format, ...) ATTR_PRINTF(1,2);
static char *xstrconcat (const char *s1, ...) ATTR_SENTINEL(0);


/*
 * Utility functions
 */

static void
die (const char *fmt, ...)
{
  va_list arg_ptr;

  va_start (arg_ptr, fmt);
  fputs (PGMNAME": fatal: ", stderr);
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

  if (!opt_verbose && !opt_debug)
    return;

  va_start (arg_ptr, fmt);
  fputs (PGMNAME": ", stderr);
  vfprintf (stderr, fmt, arg_ptr);
  va_end (arg_ptr);
}


static void
dbg (const char *fmt, ...)
{
  va_list arg_ptr;

  if (!opt_debug)
    return;

  va_start (arg_ptr, fmt);
  fputs (PGMNAME": DBG: ", stderr);
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
xrealloc (void *a, size_t newsize)
{
  void *p = realloc (a, newsize);
  if (!p && newsize)
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
        die ("too may args for strconcat\n");
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


/* Remove leading and trailing white space from STR.  */
static char *
trim_spaces (char *string)
{
  unsigned char *s, *p, *mark;

  p = s = (unsigned char *)string;
  for (; *p && isspace (*p); p++)
    ;
  for (mark=NULL; (*s = *p); s++, p++)
    {
      if (isspace (*p))
        {
          if (!mark)
            mark = s;
        }
      else
        mark = NULL;
    }
  if (mark)
    *mark = 0;

  return string;
}


/* Read up to MAXLENGTH bytes from FP into a buffer and return that
 * buffer.  Die on error.  */
static char *
xreadfile (FILE *fp, size_t maxlength)
{
#define NCHUNK 8192
  char *buf = NULL;
  size_t buflen = 0;
  size_t nread;
  size_t bufsize = 0;

  do
    {
      bufsize += NCHUNK;
      buf = xrealloc (buf, bufsize + 1);

      nread = fread (buf+buflen, 1, NCHUNK, fp);
      if (nread < NCHUNK && ferror (fp))
        {
          err ("error reading input: %s\n", strerror (errno));
          free (buf);
          exit (1);
        }
      buflen += nread;
      if (maxlength && buflen >= maxlength)
        {
          buflen = maxlength;
          break;
        }
    }
  while (nread == NCHUNK);

  buf[buflen] = 0;
  if (strlen (buf) != buflen)
    err ("warning: Nul characters detected in the input\n");
  return buf;
#undef NCHUNK
}



/*
 * txxmpp proper
 */

static xmpp_stanza_t *
new_name_stanza (xmpp_ctx_t *ctx, const char *name)
{
  xmpp_stanza_t *stanza;
  int rc;

  stanza = xmpp_stanza_new (ctx);
  if (!stanza)
    die ("xmpp_stanza_new failed\n");
  rc = xmpp_stanza_set_name (stanza, name);
  if (rc)
    die ("xmpp_stanza_set_name failed: rc=%d\n", rc);
  return stanza;
}

static xmpp_stanza_t *
new_text_stanza (xmpp_ctx_t *ctx, const char *text)
{
  xmpp_stanza_t *stanza;
  int rc;

  stanza = xmpp_stanza_new (ctx);
  if (!stanza)
    die ("xmpp_stanza_new failed\n");
  rc = xmpp_stanza_set_text (stanza, text);
  if (rc)
    die ("xmpp_stanza_set_text failed: rc=%d\n", rc);
  return stanza;
}


const char *
get_bound_jid (const xmpp_conn_t * const conn)
{
  const char *s = xmpp_conn_get_bound_jid (conn);
  if (!s)
    die ("xmpp_conn_get_bound_jid failed\n");
  return s;
}


/* Send a standard message to RECIPIENT which already has any desired
 * resource attached.  */
static void
send_message (xmpp_ctx_t *ctx, xmpp_conn_t * const conn,
              const char *recipient)
{
  int rc;
  xmpp_stanza_t *stanza;

  inf ("sending message to '%s'\n", recipient);
  stanza = xmpp_message_new (ctx, opt_chat? "chat":"normal", recipient, NULL);
  if (!stanza)
    err ("xmpp_message_new failed for '%s'\n", recipient);
  else
    {
      rc = xmpp_message_set_body (stanza, the_message);
      if (rc)
        err ("xmpp_message_set_body failed: rc=%d\n", rc);
      else
        {
          xmpp_send (conn, stanza);
        }
      xmpp_stanza_release (stanza);
    }
}


/* Send a MUC message to RECIPIENT using NICK.  A resource has already
 * been stripped from RECIPIENT, NICK may be the empty string to
 * indicate the use of a default.  */
static void
send_muc_message (xmpp_ctx_t *ctx, xmpp_conn_t * const conn,
                  const char *recipient, const char *nick)
{
  int rc;
  xmpp_stanza_t *stanza, *stanza2, *stanza3;
  char *p;
  const char *recp;
  char *nickbuf = NULL;
  char *recpbuf = NULL;

  /* Make sure we have a NICK.  FIXME: We should first ask the server
   * whether it already has a reserved nick. */
  if (!*nick)
    {
      nickbuf = xstrdup (get_bound_jid (conn));
      p = strchr (nickbuf, '@');
      if (!p)
        die ("internal error at %d\n", __LINE__);
      *p = 0;
      nick = nickbuf;
    }

  inf ("sending MUC message to '%s' nick '%s'\n", recipient, nick);

  recp = recpbuf = xstrconcat (recipient, "/", nick, NULL);

  dbg ("sending presence to the room\n");
  stanza = xmpp_presence_new (ctx);
  if (!stanza)
    die ("xmpp_presence_new failed\n");
  rc = xmpp_stanza_set_from (stanza, get_bound_jid (conn));
  if (rc)
    die ("xmpp_stanza_set_from failed: rc=%d\n", rc);
  rc = xmpp_stanza_set_to (stanza, recp);
  if (rc)
    die ("xmpp_stanza_set_from failed: rc=%d\n", rc);
  rc = xmpp_stanza_set_id (stanza, "pres1");
  if (rc)
    die ("xmpp_stanza_set_id failed: rc=%d\n", rc);

  /* Tell server that we support the Basic MUC protocol and that we
   * don't want any history.  */
  stanza2 = new_name_stanza (ctx, "x");
  rc = xmpp_stanza_set_ns (stanza2, "http://jabber.org/protocol/muc");
  if (rc)
    die ("xmpp_stanza_set_ns failed: rc=%d\n", rc);
  stanza3 = new_name_stanza (ctx, "history");
  rc = xmpp_stanza_set_attribute (stanza3, "maxchars", "0");
  if (rc)
    die ("xmpp_stanza_set_attribute failed: rc=%d\n", rc);
  rc = xmpp_stanza_add_child (stanza2, stanza3);
  if (rc)
    die ("xmpp_stanza_add_child failed: rc=%d\n", rc);
  xmpp_stanza_release (stanza3);
  rc = xmpp_stanza_add_child (stanza, stanza2);
  if (rc)
    die ("xmpp_stanza_add_child failed: rc=%d\n", rc);
  xmpp_stanza_release (stanza2);

  xmpp_send (conn, stanza);
  xmpp_stanza_release (stanza);

  stanza = xmpp_message_new (ctx, "groupchat", recipient, "chat1");
  if (!stanza)
    err ("xmpp_message_new failed for '%s'\n", recipient);
  else
    {
      rc = xmpp_message_set_body (stanza, the_message);
      if (rc)
        err ("xmpp_message_set_body failed: rc=%d\n", rc);
      else
        {
          xmpp_send (conn, stanza);
        }
      xmpp_stanza_release (stanza);
    }

  free (nickbuf);
  free (recpbuf);
}


/* Handle iq:version stanzas.  */
static int
version_handler (xmpp_conn_t * const conn, xmpp_stanza_t * const stanza,
                 void * const opaque)
{
  xmpp_ctx_t *ctx = opaque;
  int rc;
  xmpp_stanza_t *reply, *query, *name, *version, *value;
  const char *s;

  inf ("received version request from %s\n", xmpp_stanza_get_from (stanza));

  reply = xmpp_stanza_reply (stanza);
  if (!reply)
    die ("xmpp_stanza_reply failed\n");
  xmpp_stanza_set_type (reply, "result");

  query = new_name_stanza (ctx, "query");
  s = xmpp_stanza_get_ns (xmpp_stanza_get_children (stanza));
  if (s)
    xmpp_stanza_set_ns (query, s);

  name = new_name_stanza (ctx, "name");
  rc = xmpp_stanza_add_child (query, name);
  if (rc)
    die ("xmpp_stanza_add_child failed: rc=%d\n", rc);
  xmpp_stanza_release (name);
  value = new_text_stanza (ctx, PGMNAME);
  rc = xmpp_stanza_add_child (name, value);
  if (rc)
    die ("xmpp_stanza_add_child failed: rc=%d\n", rc);
  xmpp_stanza_release (value);

  version = new_name_stanza (ctx, "version");
  rc = xmpp_stanza_add_child (query, version);
  if (rc)
    die ("xmpp_stanza_add_child failed: rc=%d\n", rc);
  xmpp_stanza_release (version);
  value = new_text_stanza (ctx, VERSION);
  rc = xmpp_stanza_add_child (version, value);
  if (rc)
    die ("xmpp_stanza_add_child failed: rc=%d\n", rc);
  xmpp_stanza_release (value);

  rc = xmpp_stanza_add_child (reply, query);
  if (rc)
    die ("xmpp_stanza_add_child failed: rc=%d\n", rc);
  xmpp_stanza_release (query);

  xmpp_send (conn, reply);
  xmpp_stanza_release (reply);

  return 1;  /* Keep this handler.  */
}


/* Handle message stanzas.  */
static int
message_handler (xmpp_conn_t * const conn, xmpp_stanza_t * const stanza,
                 void * const opaque)
{
  xmpp_ctx_t *ctx = opaque;
  const char *type;
  xmpp_stanza_t *child, *achild;
  char *subject, *body;
  const char *code, *errtype;

  type = xmpp_stanza_get_type (stanza);
  if (type && !strcmp (type, "error"))
    {
      child = xmpp_stanza_get_child_by_name (stanza, "error");
      errtype = child? xmpp_stanza_get_attribute (child, "type") : NULL;
      code = child? xmpp_stanza_get_attribute (child, "code") : NULL;
      err ("received error from <%s>: %s=%s\n",
           xmpp_stanza_get_from (stanza),
           code? "code":"type",
           code?  code : errtype);
      achild = xmpp_stanza_get_child_by_name (child, "text");
      body = achild? xmpp_stanza_get_text (achild) : NULL;
      if (body)
        inf ("->%s<-\n", body);
      xmpp_free (ctx, body);
    }
  else if (xmpp_stanza_get_child_by_name (stanza, "body"))
    {
      /* No type but has a body.  */
      child = xmpp_stanza_get_child_by_name (stanza, "subject");
      subject = child? xmpp_stanza_get_text (child) : NULL;

      child = xmpp_stanza_get_child_by_name (stanza, "body");
      body = child? xmpp_stanza_get_text (child) : NULL;

      inf ("received message from <%s> %s%s%s\n", xmpp_stanza_get_from (stanza),
           subject? "(subject: ":"",
           subject? subject:"",
           subject? ")":"");
      if (body)
        inf ("->%s<-\n", body);

      xmpp_free (ctx, body);
      xmpp_free (ctx, subject);
    }

  return 1; /* Keep this handler.  */
}


/* Handle connection events.  */
static void
conn_handler (xmpp_conn_t * const conn, const xmpp_conn_event_t status,
              const int error, xmpp_stream_error_t * const stream_error,
              void * const userdata)
{
  xmpp_ctx_t *ctx = (xmpp_ctx_t *)userdata;
  int rcpidx;
  char *recpbuffer;
  const char *recp, *nick, *s;

  if (status == XMPP_CONN_CONNECT)
    {
      inf ("connected\n");

      xmpp_handler_add (conn, version_handler,
                        "jabber:iq:version", "iq", NULL, ctx);

      xmpp_handler_add (conn, message_handler,
                        NULL, "message", NULL, ctx);

      /* Send the messages.  */
      for (rcpidx=0; (recp = opt_recipients[rcpidx]); rcpidx++)
        {
          s = strchr (recp, '/');
          if (s && s[1] == '/')  /* MUC  */
            {
              nick = s + 2;
              recp = recpbuffer = xstrdup (recp);
              *strchr (recpbuffer, '/') = 0;
              send_muc_message (ctx, conn, recp, nick);
              free (recpbuffer);
            }
          else
            send_message (ctx, conn, recp);
        }

      inf ("requesting disconnect\n");
      xmpp_disconnect (conn);
    }
  else
    {
      inf ("disconnected\n");
      xmpp_stop(ctx);
    }
}


/* Read our config file.  */
static void
read_config (void)
{
  char *fname;
  const char *s;
  FILE *fp;
  char line[512];
  int c;
  char *user, *pass;

  s = getenv ("HOME");
  if (!s)
    s = "";
  fname = xstrconcat (s, "/." PGMNAME "rc", NULL);
  fp = fopen (fname, "r");
  if (!fp)
    {
      free (fname);
      return;
    }

  user = pass = NULL;
  while (fgets (line, sizeof line, fp))
    {
      if (line[strlen (line)-1] != '\n')
        {
          while ((c = getc (fp)) != EOF && c != '\n')
            ;
          err ("warning: ignoring rest of overlong line in '%s'\n", fname);
        }
      if (*line == '#')
        continue;
      trim_spaces (line);
      if (!*line)
        continue;
      user = strtok (line, " \t");
      if (user)
        pass = strtok (NULL, " \t");
      else
        pass = NULL;

      if (!opt_user) /* Take the first line and we are done.  */
        {
          opt_user = xstrdup (user);
          if (!opt_pass && pass)
            opt_pass = xstrdup (pass);
          break;
        }

      if (!strcmp (opt_user, user) && !opt_pass)  /* Password found.  */
        {
          opt_pass = xstrdup (pass);
          break;
        }
    }

  fclose (fp);
  free (fname);
}


int
main (int argc, char **argv)
{
  int last_argc = -1;
  int rc, idx, anyerr;
  int opt_me = 0;
  unsigned long opt_limit = 0;
  const char *recp;
  xmpp_ctx_t *ctx;
  xmpp_conn_t *conn;

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
          fputs ("usage: " PGMNAME " [options] recipients\n"
                 "Send XMPP message taken from stdin to the recipients.\n\n"
                 "Options:\n"
                 "  --version         print program version\n"
                 "  --verbose         verbose diagnostics\n"
                 "  --debug           flyswatter\n"
                 "  --subject STRING  use STRING as subject\n"
                 "  --chat            Use \"chat\" as message type\n"
                 "  --me              Prepend \"/me \" to the message\n"
                 "  --user JID        connect as JID\n"
                 "  --pass PASS       override password with PASS\n"
                 "  --resource RES    override default resource with RES\n"
                 "  --limit N         read not more than N bytes\n"
                 "\n"
                 "The password is taken from the ~/.txmpprc file where the\n"
                 "first non-comment line specifies the default user:\n"
                 "  ----- 8< ----- 8< ----- 8< -----\n"
                 "  # Example config for txxmppp\n"
                 "  foo@jabber.example.org PASSWORD\n"
                 "  bar@example.net PASSWORD\n"
                 "  ----- >8 ----- >8 ----- >8 -----\n"
                 "To send to a MUC use resource with a leading slash followed\n"
                 "by the nick (e.g. \"juliet@capulet.lit.org//Giulietta\").\n"
                 "\n", stdout);
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
      else if (!strcmp (*argv, "--user"))
        {
          argc--; argv++;
          if (!argc || !**argv || !strcmp (*argv, "--"))
            die ("argument missing for option '%s'\n", argv[-1]);
          opt_user = *argv;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--pass"))
        {
          argc--; argv++;
          if (!argc || !**argv || !strcmp (*argv, "--"))
            die ("argument missing for option '%s'\n", argv[-1]);
          opt_pass = *argv;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--resource"))
        {
          argc--; argv++;
          if (!argc || !**argv || !strcmp (*argv, "--"))
            die ("argument missing for option '%s'\n", argv[-1]);
          opt_resource = *argv;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--subject"))
        {
          argc--; argv++;
          if (!argc || !**argv || !strcmp (*argv, "--"))
            die ("argument missing for option '%s'\n", argv[-1]);
          opt_subject = *argv;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--chat"))
        {
          opt_chat = 1;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--me"))
        {
          opt_me = 1;
          argc--; argv++;
        }
      else if (!strcmp (*argv, "--limit"))
        {
          argc--; argv++;
          if (!argc || !**argv || !strcmp (*argv, "--"))
            die ("argument missing for option '%s'\n", argv[-1]);
          opt_limit = strtoul (*argv, NULL, 0);
          argc--; argv++;
        }
      else if (!strncmp (*argv, "--", 2))
        die ("unknown option '%s' (use --help)\n", *argv);
    }

  if (!argc)
    die ("usage: " PGMNAME " [options] recipients  (try --help)\n");

  opt_recipients = argv;

  read_config ();

  anyerr = 0;
  if (!opt_user || !*opt_user || !opt_pass || !*opt_pass)
    {
      if (!opt_user || !*opt_user)
        err ("error: no user given\n");
      if (!opt_pass || !*opt_pass)
        err ("error: no password given\n");
      inf ("hint: use config file \"~/.txxmpprc\" or option \"--user\"\n");
      anyerr = 1;
    }

  for (idx = 0; (recp = opt_recipients[idx]); idx++)
    {
      const char *at, *slash;
      at = strchr (recp, '@');
      slash = strchr (recp, '/');
      if (!at || at == recp || !at[1]
          || (slash && (slash < at || at + 1 == slash)))
        {
          err ("error: invalid recipient '%s'\n", recp);
          anyerr = 1;;
        }
    }

  if (anyerr)
    exit (1);

  the_message = xreadfile (stdin, opt_limit);
  if (opt_me)
    {
      char *newbuf = xstrconcat ("/me ", the_message, NULL);
      free (the_message);
      the_message = newbuf;
    }

  xmpp_initialize ();

  ctx = xmpp_ctx_new (NULL,
                      (opt_debug? xmpp_get_default_logger (XMPP_LEVEL_DEBUG)
                       /* */    : NULL));
  if (!ctx)
    die ("xmpp_ctx_new failed\n");

  conn = xmpp_conn_new (ctx);
  if (!conn)
    die ("xmpp_conn_new failed\n");

  xmpp_conn_set_jid (conn, opt_user);
  xmpp_conn_set_pass (conn, opt_pass);

  rc = xmpp_connect_client (conn, NULL, 0, conn_handler, ctx);
  if (rc)
    err ("xmpp_connect_client failed: rc=%d\n", rc);
  else
    {
      xmpp_run (ctx);
    }

  xmpp_conn_release (conn);
  xmpp_ctx_free (ctx);

  xmpp_shutdown ();

  free (the_message);
  the_message = NULL;

  return 0;
}


/*
Local Variables:
compile-command: "gcc -Wall -g -lstrophe -o txxmpp txxmpp.c"
End:
*/
