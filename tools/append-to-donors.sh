#!/bin/sh
# append-to-donors.sh
# Append new names from the payproc journal to the donors file
# and send a Thank You mail.
#
# Note that this script does not yet handle subscriptions.  Because we
# want to verify the mail address anyway, it makes sense to move mai
# sending to payprocd.  The final plan is to use webhooks to create
# charge records and use them to add a Donor reulary to the list of
# donors (but may be limited to once a year)

pgm="append-to-donors.sh"
set -e

# We temporary need the next line due to an libgpg-error update
LD_LIBRARY_PATH=/usr/local/lib
export LD_LIBRARY_PATH

PATH=/usr/local/bin:$PATH
SENDMAIL="/usr/sbin/sendmail"
LC_ALL=C
LC_CTYPE=C
RFCDATE="$(date -R)"
SIGDELIM="-- "

usage()
{
    cat <<EOF
Usage: $pgm [OPTIONS]
Options:
        --verbose  Run in verbose mode
	--force    Force re-creation of files.
        --test     Run in test environment (preview.gnupg.org)
EOF
    exit $1
}


verbose=no
force=no
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
        --verbose)
            verbose=yes
            ;;
	--force)
	    force=yes
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


if [ $testmode = yes ]; then
  htdocs="/var/www/www/preview.gnupg.org/htdocs"
  journal_dir="/var/log/payproc-test"
else
  htdocs="/var/www/www/www.gnupg.org/htdocs"
  journal_dir="/var/log/payproc"
fi

donors="$htdocs/donate/donors.dat"
donations="$htdocs/donate/donations.dat"

LOCKFILE="$donors.lock"

if [ ! -f "$donors" ]; then
  echo "$pgm: '$donors' not found" >&2;
  exit 1
fi

if [ x"$(idn --quiet wk@gnupg.org)" != x"wk@gnupg.org" ]; then
  echo  "$pgm: idn(1) tool not installed or not working"
  exit 1
fi
if [ x"$(mu-tool 2047 -c utf-8 '<wk@gnupg.org>')" \
      != x"=?utf-8?Q?<wk@gnupg.org>?=" ]; then
  echo "$pgm: mu-tool(1) tool not installed or not working"
  exit 1
fi


if ! lockfile -l 7200 -r 2 $LOCKFILE; then
    echo "$pgm: another instance is still running"
    exit 0
fi
trap "rm -f $LOCKFILE $donors.tmp $donors.stamp" 0


# Send a thank you mail
# Uses these variables:
#  amount   - The amount of the donation
#  currency - The currency for the amount
#  euro     - The amount cinvertet to Euro
#  xmail    - The mailbox
#  name     - The name or empty for an anonymous donation
#  message  - The message to us or empty
# Used scratch variables:
#  upcurrency
#  ineuro
#  xamount
#
# FIXME: Clean message and name and use an appropriate encoding.
#        The second mail should actually be encrypted.  In fact
#        we would better try to encrypt also the first mail.  Add a
#        pubkey field to the donation page?
#
send_thanks () {
    if [ $1 -gt 0 ]; then
        recurring="recurring "
    else
        recurring=""
    fi
    upcurrency=$(echo $currency | tr [a-z] [A-Z])
    if [ "$upcurrency" = EUR ]; then
        ineuro=
    else
        ineuro=" (about $(echo $euro| awk '{print int($0 + 0.5)}') EUR)"
    fi
    xamount="$(echo $amount| awk '{print int($0 + 0.5)}')"
    if [ -n "$xmail" ]; then
      xidnmail=$(CHARSET=UTF-8 idn --no-tld --quiet "$xmail")
    else
      xidnmail=""
    fi
    if [ x"$xidnmail" = x"$xmail" ]; then
      xqpmail="$xmail"
    else
      xqpmail=$(mu-tool 2047 -c utf-8 "$xmail")
    fi
    if [ $testmode = yes ]; then
      xisatest="[TEST DONATION] "
    else
      xisatest=""
    fi
    ( cat <<EOF
From: donations@gnupg.org
To: $xqpmail
Subject: ${xisatest}Thank you for supporting GnuPG
Date: $RFCDATE
Mime-Version: 1.0
Content-Type: text/plain
X-Loop: gnupg-donations-thanks.gnupg.org

Dear ${name:-Anonymous},

we received $xamount $upcurrency$ineuro as a ${recurring}donation to the GnuPG project.
Your donation helps us to develop and maintain GnuPG and related software.

Thank you.

  The GnuPG Team


$SIGDELIM
GnuPG - helping to keep private data private
EOF
    ) | $SENDMAIL -oi donations@gnupg.org "$xidnmail"


if [ -n "$message" ]; then
    ( cat <<EOF
From: donations@gnupg.org
To: donations@gnupg.org
Reply-To: $xqpmail
Subject: ${xisatest}Message from GnuPG donor
Date: $RFCDATE
Mime-Version: 1.0
Content-Type: text/plain
X-Loop: gnupg-donations-thanks.gnupg.org

Name ..: ${name:-Anonymous}
Amount : $amount $upcurrency $ineuro
Message: $message
EOF
    ) | $SENDMAIL -oi donations@gnupg.org
fi

}


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


[ -f "$donors".stamp ] && rm "$donors".stamp
cat "$donors" > "$donors.tmp"
find $journal_dir/ -type f -name 'journal-????????.log' -print \
     | sort | while read fname; do
    fname=$(basename "$fname")
    jdate=${fname%.log}
    jdate=${jdate#journal-}
    jyear=$(echo $jdate |sed 's/\(....\).*/\1/')
    if [ "$jdate" -ge "$lastdate" ]; then
        [ "$jdate" -gt "$lastdate" ] && lastline=0
        # First for charge records
        payproc-jrnl -F_lnr -Fdate -F'[name]' -F'[message]' \
                     -Fmail -Famount -Fcurrency -Feuro\
           -S "_lnr > $lastline" -Stype=C -Saccount==1 \
           --html --print "$journal_dir/journal-$jdate.log" \
         | while IFS=: read lnr datestr name message \
                            xmail amount currency euro rest; do
            name=$(echo "$name" | tr \`\$: ...)
            message=$(echo "$message" | tr \`\$ ..)
            xmail=$(echo "$xmail" | tr \`\$ .. | sed 's/\.$//')
            # Note that we removed colons from $name
            echo "$jyear:$datestr:$name::$lnr:" >> "$donors.tmp"
            touch "$donors".stamp
            send_thanks 0
         done
        # Second for new subscriptions
        payproc-jrnl -F_lnr -Fdate -F'[name]' -F'[message]' \
                     -Fmail -Famount -Fcurrency -Feuro -Frecur\
           -S "_lnr > $lastline" -Stype=S -Saccount==1 \
           --html --print "$journal_dir/journal-$jdate.log" \
         | while IFS=: read lnr datestr name message \
                            xmail amount currency euro recur rest; do
            name=$(echo "$name" | tr \`\$: ...)
            message=$(echo "$message" | tr \`\$ ..)
            xmail=$(echo "$xmail" | tr \`\$ .. | sed 's/\.$//')
            # Note that we removed colons from $name
            echo "$jyear:$datestr:$name:S:$lnr:" >> "$donors.tmp"
            touch "$donors".stamp
            send_thanks 1
         done
    fi
done

# If we have any new records update the files.
if [ -f "$donors".stamp -o $force = yes ]; then

  if ! mv "$donors.tmp" "$donors"; then
    echo "$pgm: error updating $donors" >&2
    exit 1
  fi

  if [ -f "$donations" ]; then
    payproc-stat -u "$donations" -- > "$donations".tmp  \
      $(find $journal_dir/ -type f -name 'journal-????????.log' -print|sort)
    if ! mv "$donations".tmp "$donations"; then
        echo "$pgm: error updating $donations" >&2
        exit 1
    fi
  else
    payproc-stat -u "$donations" -- > "$donations"  \
      $(find $journal_dir/ -type f -name 'journal-????????.log' -print|sort)
  fi
fi
