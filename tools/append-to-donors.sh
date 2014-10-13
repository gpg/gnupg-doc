#!/bin/sh
# append-to-donors.sh
# Append new names from the payproc journal to the donors file
# and send a Thank You mail.

pgm="append-to-donors.sh"
set -e

PATH=/usr/local/bin:$PATH
SENDMAIL="/usr/sbin/sendmail"
LC_ALL=C
LC_CTYPE=C
RFCDATE="$(date -R)"
SIGDELIM="-- "

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
#
# FIXME: Clean message and name and use an appropriate encoding.
#        The second mail should actually be encrypted.  In fact
#        we would better try to encrypt also the first mail.  Add a
#        pubkey field to the donation page?
#
send_thanks () {
    upcurrency=$(echo $currency | tr [a-z] [A-Z])
    if [ "$upcurrency" = EUR ]; then
        ineuro=
    else
        ineuro=" (about $euro EUR)"
    fi
    ( cat <<EOF
From: donations@gnupg.org
To: $xmail
Subject: Thank you for supporting GnuPG
Date: $RFCDATE
Mime-Version: 1.0
Content-Type: text/plain
X-Loop: gnupg-donations-thanks.gnupg.org

Dear ${name:-Anonymous},

we received $amount $upcurrency$ineuro as a donation for the GnuPG project.
Your donation helps us to develop and maintain GnuPG and related software.

Thank you.

  Werner


$SIGDELIM
GnuPG - helping to keep private data private
EOF
    ) | $SENDMAIL -oi donations@gnupg.org "$xmail"


if [ -n "$message" ]; then
    ( cat <<EOF
From: donations@gnupg.org
To: donations@gnupg.org
Reply-To: $xmail
Subject: Message from GnuPG donor
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

cat "$donors" > "$donors.tmp"
find $journal_dir -type f -name 'journal-????????.log' -print \
     | sort | while read fname; do
    fname=$(basename "$fname")
    jdate=${fname%.log}
    jdate=${jdate#journal-}
    jyear=$(echo $jdate |sed 's/\(....\).*/\1/')
    if [ "$jdate" -ge "$lastdate" ]; then
        [ "$jdate" -gt "$lastdate" ] && lastline=0
        payproc-jrnl -F_lnr -Fdate -F'[name]' -F'[message]' \
                     -Fmail -Famount -Fcurrency -Feuro\
           -S "_lnr > $lastline" -Stype=C -Saccount==1 \
           --html --print "$journal_dir/journal-$jdate.log" \
         | while IFS=: read lnr datestr name message \
                            xmail amount currency euro rest; do
            name=$(echo "$name" | tr \`\$: ...)
            message=$(echo "$message" | tr \`\$ ..)
            xmail=$(echo "$xmail" | tr \`\$ ..)
            if [ -n "$name" ]; then
               # Note that we removed colons from $name
               echo "$jyear:$datestr:$name::$lnr:" >> "$donors.tmp"
            fi
            send_thanks
         done
    fi
done
if ! mv "$donors.tmp" "$donors"; then
  echo "$pgm: error updating $donors" >&2
  exit 1
fi
