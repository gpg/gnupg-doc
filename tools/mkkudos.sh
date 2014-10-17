#!/bin/sh

set -e

htdocs="/var/www/www/www.gnupg.org/htdocs"
#htdocs="/home/wk/s/gnupg-doc/web"

donors="$htdocs/donate/donors.dat"
donations="$htdocs/donate/donations.dat"


usage()
{
    cat <<EOF
Usage: $0 [OPTIONS]
Options:
	--force    Force re-creation of files.
EOF
    exit $1
}


force=no
while [ $# -gt 0 ]; do
    case "$1" in
	# Set up `optarg'.
	--*=*)
	    optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'`
	    ;;
	*)
	    optarg=""
	    ;;
    esac

    case $1 in
	--force)
	    force=yes
	    ;;
        --help)
            usage 0
            ;;
	*)
	    usage 1 1>&2
	    ;;
    esac
    shift
done


if [ ! -f "$donors" ]; then
  echo "mkkudos.sh: '$donors' not found" >&2;
  exit 1
fi
if [ ! -f "$donations" ]; then
  echo "mkkudos.sh: '$donations' not found" >&2;
  exit 1
fi

tmp=$(head -1 "$donations")
monyear=$(echo "$tmp" | awk -F: 'BEGIN { m[1] = "January";
      m[2] = "February"; m[3] = "March"; m[4] = "April"; m[5] = "May";
      m[6] = "June"; m[7] = "July"; m[8] = "August"; m[9] = "September";
      m[10] = "October"; m[11] = "November"; m[12] = "December"; }
      {printf "%s %d", m[$2] , $1}')
euro=$(echo "$tmp" | awk -F: '{printf "%d Euro", int($8 + 0.5)}')
euroyr=$(echo "$tmp" | awk -F: '{printf "%d Euro", int($10 + 0.5)}')
n=$(echo "$tmp" | awk -F: '{printf "%d", $7}')
nyr=$(echo "$tmp" | awk -F: '{printf "%d", $9}')

for file in "$htdocs/donate/"kudos-????.html "$htdocs/donate/"kudos.html; do
   if [ $force = no ]; then
     [ "$file" -ot "$donors" ] || continue
   fi
   if [ "$file" = "$htdocs/donate/"kudos.html ]; then
     year=$(date +%Y)
   else
     year=${file#$htdocs/donate/kudos-}
     year=${year%.html}
   fi
   echo "processing $file" >&2
   [ -f "$file.tmp" ] && rm "$file.tmp"
   awk -F: -v year=$year -v donors="$donors" \
           -v monyear="$monyear" -v euro="$euro" -v euroyr="$euroyr" \
           -v n="$n" -v nyr="$nyr" \
            <"$file"  >"$file.tmp" '
     /<!--BEGIN-DONATIONS-->/ {indon=1; print; insert("") }
     /<!--END-DONATIONS-->/ {indon=0}
     /<!--BEGIN-DONATIONS_goteo13-->/ {indon=1; print; insert("goteo13") }
     /<!--END-DONATIONS_goteo13-->/ {indon=0}
     /<!--INSERT-MONTH-DATE-->/ {
           printf "<!--INSERT-MONTH-DATE--> %s\n", monyear;
           next
     }
     /<!--INSERT-MONTH-EURO-->/ {
           printf "<!--INSERT-MONTH-EURO--> %s\n", euro;
           next
     }
     /<!--INSERT-MONTH-N-->/ {
           printf "<!--INSERT-MONTH-N--> %s\n", n;
           next
     }
     /<!--INSERT-YEAR-EURO-->/ {
           printf "<!--INSERT-YEAR-EURO--> %s\n", euroyr;
           next
     }
     /<!--INSERT-YEAR-N-->/ {
           printf "<!--INSERT-YEAR-N--> %s\n", nyr;
           next
     }
     !indon { print }

     function insert (tag) {
       while (getline < donors) {
         if ( $0 ~ /^(#.*)?$/ )
            continue;
         if ( $3 == "" )
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
