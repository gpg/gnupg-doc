#!/bin/sh

set -e

usage()
{
    cat <<EOF
Usage: $0 [OPTIONS]
Options:
	--force    Force re-creation of files.
        --verbose  Run in verbose mode
        --test     Run in test environment
EOF
    exit $1
}


force=no
verbose=no
testmode=no
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
        --verbose)
            verbose=yes
            ;;
        --test)
            testmode=yes
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

htdocs="/var/www/www/www.gnupg.org/htdocs"
donors="$htdocs/donate/donors.dat"
donations="$htdocs/donate/donations.dat"

if [ $testmode = yes ]; then
  htdocs="/home/wk/s/gnupg-doc/stage"
  donors="$htdocs/../scratch/donors.dat"
  donations="$htdocs/../scratch/donations.dat"
fi


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
euro=$(echo "$tmp" | awk -F: '{printf "%d &euro;", int($8 + 0.5)}')
euroyr=$(echo "$tmp" | awk -F: '{printf "%d &euro;", int($10 + 0.5)}')
n=$(echo "$tmp" | awk -F: '{printf "%d", $7}')
nyr=$(echo "$tmp" | awk -F: '{printf "%d", $9}')
goal="120000"
percent=$(echo "$euro:$goal" | awk -F: '{printf "%d",(int($1)*100)/int($2)}')

for file in "$htdocs/donate/"kudos-????.html "$htdocs/donate/"kudos.html \
            "$htdocs/donate/"index.html \
            "$htdocs/"index.html
 do
   if [ $force = no ]; then
     [ "$file" -ot "$donors" ] || continue
   fi
   if [ "$file" = "$htdocs/donate/"kudos.html ]; then
     year=$(date +%Y)
   else
     year=${file#$htdocs/donate/kudos-}
     year=${year%.html}
   fi
   [ $verbose = yes ] && echo "processing $file" >&2
   [ -f "$file.tmp" ] && rm "$file.tmp"
   awk -F: -v year=$year -v donors="$donors" \
           -v monyear="$monyear" -v euro="$euro" -v euroyr="$euroyr" \
           -v n="$n" -v nyr="$nyr" -v goal="$goal" -v percent="$percent" \
            <"$file"  >"$file.tmp" '
     /<!--BEGIN-DONATIONS-->/ {indon=1; print; insert("") }
     /<!--END-DONATIONS-->/ {indon=0}
     /<!--BEGIN-SOME-DONATIONS-->/ {indon=1; print; insertsome("") }
     /<!--END-SOME-DONATIONS-->/ {indon=0}
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
     /<!--INSERT-PROGRESS-LEFT-->/ {
           printf "<!--INSERT-PROGRESS-LEFT-->%s\n",
                  euro;
           next
     }
     /<!--INSERT-PROGRESS-RIGHT-->/ {
           printf "<!--INSERT-PROGRESS-RIGHT-->goal: %s &euro;\n", goal;
           next
     }
     /<!--REPLACE-PROGRESS-PERCENT-->/ {
           printf "style=\"width: %d%%\"<!--REPLACE-PROGRESS-PERCENT-->\n",
                  percent;
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

     function insertsome (tag) {
       i = 0
       while (getline < donors) {
         if ( $0 ~ /^(#.*)?$/ )
            continue;
         if ( $3 == "" )
            continue;
         if ($4==tag) {
           data[i++] = $3
         }
       }
       close (donors)
       j = i > 16 ? ( i - 16 ) : 0
       while (j < i) {
           printf "<li>%s</li>\n", data[j++]
       }
     }
     '
   mv "$file.tmp" "$file" || echo "mkkudos.sh: error updating $file" >&2
done
