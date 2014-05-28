#!/bin/sh

set -e

htdocs="/var/www/www/www.gnupg.org/htdocs"
#htdocs="/home/wk/s/gnupg-doc/web"

donors="$htdocs/donate/donors.dat"

if [ ! -f "$donors" ]; then
  echo "mkkudos.sh: '$donors' not found" >&2;
  exit 1
fi

for file in "$htdocs/donate/"kudos-????.html "$htdocs/donate/"kudos.html; do
   [ "$file" -ot "$donors" ] || continue
   if [ "$file" = "$htdocs/donate/"kudos.html ]; then
     year=$(date +%Y)
   else
     year=${file#$htdocs/donate/kudos-}
     year=${year%.html}
   fi
   echo "processing $file" >&2
   [ -f "$file.tmp" ] && rm "$file.tmp"
   awk -F: -v year=$year -v donors="$donors" <"$file"  >"$file.tmp" '
     /<!--BEGIN-DONATIONS-->/ {indon=1; print; insert("") }
     /<!--END-DONATIONS-->/ {indon=0}
     /<!--BEGIN-DONATIONS_goteo13-->/ {indon=1; print; insert("goteo13") }
     /<!--END-DONATIONS_goteo13-->/ {indon=0}
     !indon { print }

     function insert (tag) {
       while (getline < donors) {
         if ( $0 ~ /^(#.*)?$/ )
            continue;
         if ($1==year && $4==tag) {
           printf "<li>%s</li>\n", $3
         }
       }
       close (donors)
     }
     '
   mv "$file.tmp" "$file" || echo "mkkudos.sh: error updating $file" >&2
done
