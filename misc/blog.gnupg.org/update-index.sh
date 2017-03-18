#!/bin/sh
# Insert the ctual index into index.html

pgm=update-index.sh

if [ ! -f index.html ]; then
  echo "$pgm: blog.gnupg.org/index.html has not been build" >&2
  exit 1
fi

# Find all rendered HTML files but skip possible translated versions.
find . -maxdepth 1 -type f -name "20*.html" -print \
    | grep -v '\.[a-z][a-z].html$' | sort -r >index.tmp
newest=$(head -1 index.tmp)

# Extract the head lines
: >index.headlines.tmp
cat index.tmp | while read fname; do
  echo -n "${fname#./}|" >>index.headlines.tmp
  sed  -n '/^<h2 id=/ {s,^<[^>]*>\(.*\)</h2>,\1,p;q}' \
       $fname >>index.headlines.tmp
done

# Update the index file
awk -F:  <index.html  >index.tmp \
  -v newest=${newest#./} '
   /<!--BEGIN-NEWEST-ENTRY-->/ {indon=1; print; insertnewest() }
   /<!--END-NEWEST-ENTRY-->/ {indon=0}
   /<!--BEGIN-BLOG-INDEX-->/ {indon=1; print; insertindex() }
   /<!--END-BLOG-INDEX-->/   {indon=0}
   !indon { print }

   function insertnewest () {
     inblog = 0
     while (getline < newest) {
       if (match ($0, /^<main>/))  { inblog = 1; continue; }
       if (match ($0, /^<\/main>/)) { inblog = 0; break; }
       if (! inblog) { continue }
       if (match ($0, /^<div id="content">/)) { continue; }
       if (match ($0, /^<\/div><!-- end content -->/)) { continue; }
       print $0
     }
    close(newest)
   }

   function insertindex (tag) {
     file = "index.headlines.tmp";
     print "<ul>"
     while (getline < file) {
       split($0, a, "|")
       printf "  <li><a href=\"%s\">%s</a>\n", a[1], a[2];
     }
     print "</ul>"
     close (file)
   }
   '
if ! mv index.tmp index.html ; then
    echo "$pgm: error updating blog index" >&2
    exit 1
fi

# FIXME: Create a feed file

# Rename headlines file
mv index.headlines.tmp headlines.txt

exit 0
