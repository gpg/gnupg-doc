#!/bin/sh
# append-to-donors.sh
# Append new names from the payproc journal tothe donros file.

pgm="append-to-donors.sh"
set -e


htdocs="/var/www/www/www.gnupg.org/htdocs"

donors="$htdocs/donate/donors.dat"
journal_dir="/var/log/payproc"
LOCKFILE="$donors.lock"

if [ ! -f "$donors" ]; then
  echo "$pgm: '$donors' not found" >&2;
  exit 1
fi

if ! lockfile -l 7200 -r 2 $LOCKFILE; then
    echo "$pgm: another instance is still running"
    exit 0
fi
trap "rm -f $LOCKFILE" 0


# Find the last entry in donors which we have put in.
tmp=$(awk -F: <$donors '
    /^(#.*)?$/ {next}
    $5!="" { date=$2;lineno=$5 }
    END {gsub(/-/,"",date)
         print date ":" lineno}
')
lastdate=$(echo $tmp | cut -d: -f1 | sed 's/T.*//')
lastline=$(echo $tmp | cut -d: -f2)
[ -z "$lastdate" ] && lastdate=19700101
[ -z "$lastline" ] && lastline=0

cat "$donors" > "$donors.tmp"
find $journal_dir -type f -name 'journal-????????.log' -print \
     | sort | while read fname; do
    fname=$(basename "$fname")
    jdate=${fname%.log}
    jdate=${jdate#journal-}
    jyear=$(echo $jdate |sed 's/\(....\).*/\1/')
    if [ "$jdate" -ge "$lastdate" ]; then
        payproc-jrnl -F_lnr -Fdate -F'[name]' \
           -S "_lnr > $lastline" -Stype=C -Saccount==1 \
           --html --print "$journal_dir/journal-$jdate.log" \
         | while IFS=: read lnr datestr name rest; do
            if [ -n "$name" ]; then
               echo "$jyear:$datestr:$name::$lnr:" >> "$donors.tmp"
            fi
         done
    fi
done
if ! mv "$donors.tmp" "$donors"; then
  echo "$pgm: error updating $donors" >&2
  exit 1
fi
