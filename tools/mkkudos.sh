#!/bin/sh

# Update the list of donors and a few other things.
#
# ====================================================================
# This org-mode snippet is used to insert the progress bar into a HTML
# file:
#
#  #+BEGIN_HTML
#  <div class="donation-progress">
#    <div class="donation-progress-bar"
#  style="width: 0%"><!--REPLACE-PROGRESS-PERCENT-->
#  <p>&nbsp;</p></div>
#    <p><span style="float: left">
#  <!--INSERT-PROGRESS-LEFT-->
#    </span>
#    <span style="float: right">
#  <!--INSERT-PROGRESS-RIGHT-->
#    </span>
#  </div>
#  <p class="doclear" style="clear: both"></p>
#  #+END_HTML
#
# To use it the code at "Campaign data" below needs to be adjusted as
# well.
# ===================================================================

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
    exit "$1"
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
blogheadlinefile="/var/www/www/blog.gnupg.org/htdocs/headlines.txt"

if [ $testmode = yes ]; then
  htdocs="/home/wk/s/gnupg-doc/stage"
  donors="$htdocs/../scratch/donors.dat"
  donations="$htdocs/../scratch/donations.dat"
  blogheadlinefile="$htdocs/../misc/blog.gnupg.org/headlines.txt"
fi


if [ ! -f "$donors" ]; then
  echo "mkkudos.sh: '$donors' not found" >&2;
  exit 1
fi
if [ ! -f "$donations" ]; then
  echo "mkkudos.sh: '$donations' not found" >&2;
  exit 1
fi

if [ ! -f "$blogheadlinefile" ]; then
  echo "mkkudos.sh: '$blogheadlinefile' not found" >&2;
  blogheadline=""
else
  blogheadline=$(awk -F\| '
        NR<=3 {printf "<li><a href=\"blog/%s\">%s</a></li>", $1, $2}
     ' "$blogheadlinefile")
fi

tmp=$(head -1 "$donations")
monyear=$(echo "$tmp" | awk -F: 'BEGIN { m[1] = "January";
      m[2] = "February"; m[3] = "March"; m[4] = "April"; m[5] = "May";
      m[6] = "June"; m[7] = "July"; m[8] = "August"; m[9] = "September";
      m[10] = "October"; m[11] = "November"; m[12] = "December"; }
      {printf "%s %d", m[int($2)] , $1}')
thisyear=$(echo "$tmp" | awk -F: '{print $1}')
euroyr=$(echo "$tmp" | awk -F: '{printf "%d &euro;", int($10 + 0.5)}')
nyr=$(echo "$tmp" | awk -F: '{printf "%d", $9}')


dontable=$(awk -F: <"$donations" -v thisyear="$thisyear" '
  BEGIN { m[1] = "January";
          m[2] = "February"; m[3] = "March"; m[4] = "April"; m[5] = "May";
          m[6] = "June"; m[7] = "July"; m[8] = "August"; m[9] = "September";
          m[10] = "October"; m[11] = "November"; m[12] = "December" ;
          printf "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\"";
          printf " rules=\"groups\" frame=\"hsides\">\n";
          printf "<colgroup>\n";
          printf "<col class=\"left\" />\n";
          printf "<col class=\"right\" />\n";
          printf "<col class=\"right\" />\n";
          printf "</colgroup>\n";
          printf "<thead>\n";
          printf "<tr>\n";
          printf "<th scope=\"col\" class=\"left\">Month</th>\n";
          printf "<th scope=\"col\" class=\"right wideright\">#</th>\n";
          printf "<th scope=\"col\" class=\"right wideright\">&euro;</th>\n";
          printf "</tr>\n";
          printf "</thead>\n";
          printf "<tbody>\n";
        }
  NR==1 { nyear = $9; totalyear = int($10 + 0.5);
        }
  $1 != thisyear {
          printf "</tbody>\n";
          printf "<tbody>\n";
          printf "<tr><td class=\"left\">%d</td>\n", thisyear;
          printf "    <td class=\"right wideright\">%d</td>\n", nyear;
          printf "    <td class=\"right wideright\">%d</td></tr>\n", totalyear;
          printf "</tbody>\n";
          printf "</table>\n";
          exit 0
        }
        { printf "<tr><td class=\"left\">%s</td>\n", m[int($2)];
          printf "    <td class=\"right wideright\">%d</td>\n", $7;
          printf "    <td class=\"right wideright\">%d</td></tr>\n",
                                                  int($8 + 0.5);
        }
')



# Campaign data
goal="120000"
percent=$(echo "$euroyr:$goal" | awk -F: '{ p = (int($1)*100)/int($2);
                                          if(p > 100) { p = 100 };
                                          printf "%d", p}')

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
   awk -F: -v year="$year" -v donors="$donors" -v dontable="$dontable" \
           -v monyear="$monyear" -v thisyear="$thisyear" \
           -v euro="$euro" -v euroyr="$euroyr" \
           -v nyr="$nyr" -v goal="$goal" -v percent="$percent" \
           -v blogheadline="$blogheadline" \
            <"$file"  >"$file.tmp" '
     /<!--BEGIN-DONATIONS-->/ {indon=1; print; insert("") }
     /<!--END-DONATIONS-->/ {indon=0}
     /<!--BEGIN-SOME-DONATIONS-->/ {indon=1; print; insertsome("") }
     /<!--END-SOME-DONATIONS-->/ {indon=0}
     /<!--BEGIN-DONATIONS_goteo13-->/ {indon=1; print; insert("goteo13") }
     /<!--END-DONATIONS_goteo13-->/ {indon=0}
     /<!--BEGIN-DONATION-TABLE-->/ {indon=1; print; print dontable }
     /<!--END-DONATION-TABLE-->/ {indon=0}
     /<!--INSERT-MONTH-DATE-->/ {
           printf "<!--INSERT-MONTH-DATE--> %s\n", monyear;
           next
     }
     /<!--INSERT-THIS-YEAR-->/ {
           printf "<!--INSERT-THIS-YEAR--> %d\n", thisyear;
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
           printf "<!--INSERT-PROGRESS-LEFT-->%s &euro;\n",
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
     /<!--INSERT-BLOG-HEADLINE-->/ {
           printf "<!--INSERT-BLOG-HEADLINE--> %s\n", blogheadline;
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
