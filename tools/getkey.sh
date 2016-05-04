#!/bin/sh
# Retrieve a key from the keyserver and store it as a local file.
# An option allows to run this as cronjob to refresh the existing
# keys from the keyserver

set -e

pgm="$0"

htdocs="/var/www/alberti/k.gnupg.net/htdocs"
keys="/var/www/alberti/k.gnupg.net/keys"
keyserver="hkp://keys.mayfirst.org"

usage()
{
    cat <<EOF
Usage: $0 [OPTIONS] [FINGERPRINTS]
Options:
        --cron     Refresh all keys
        --verbose  Run in verbose mode
        --test     Run in test environment
EOF
    exit "$1"
}


cronmode=no
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

    case "$1" in
	--cron)
	    cronmode=yes
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
	--*)
	    usage 1 1>&2
	    ;;
        *)
            break
    esac
    shift
done


if [ ! -d "$htdocs" ]; then
  echo "$pgm: '$htdocs' not found" >&2;
  exit 1
fi
if [ ! -d "$keys" ]; then
  echo "$pgm: '$keys' not found" >&2;
  exit 1
fi
if [ -z "$keyserver" ]; then
  echo "$pgm: No keyserver configured" >&2;
  exit 1
fi
if [ $cronmode = yes ]; then
  if [ $# -ne 0 ]; then
    echo "$pgm: Unexpected arguments in cron mode" >&2
    usage 1
  fi
else
  if [ $# -eq 0 ]; then
    echo "$pgm: No arguments given" >&2
    usage 1
  fi
fi
if ! cd "$keys" ; then
  echo "$pgm: cd to '$keys' failed" >&2
  exit 1
fi

GNUPGHOME="$keys"
export GNUPGHOME
gpgopts="--no-permission-warning --quiet"


errors=no
# Note that the loop is not entered in cron mode
while [ $# -gt 0 ]; do
    fpr="$1"
    shift
    b=$(echo "$fpr" | tr -cd '[:xdigit:]')
    if [ ${#fpr} -lt 40 -o ${#fpr} -ne ${#b} ]; then
        echo "$pgm: '$fpr' does not look like a fingerprint" >&2
        errors=yes
        continue
    fi
    [ $verbose = yes ] && echo "$pgm: Retrieving $fpr ..." >&2
    if ! gpg $gpgopts --keyserver $keyserver --recv-key $fpr ; then
        echo "$pgm: error retrieving '$fpr'" >&2
        errors=yes
        continue
    fi
    [ $verbose = yes ] && echo "$pgm: Storing $fpr ..." >&2
    if ! gpg $gpgopts --export --yes --output "$htdocs/$fpr" $fpr ; then
        echo "$pgm: error storing '$fpr'" >&2
        errors=yes
        continue
    fi
done


if [ "$cronmode" = yes ]; then
  find "$htdocs" -type f -name '[0-9A-F][0-9A-F]*[0-9A-F]' -printf '%P\n' \
    | ( while read fpr rest ; do
        b=$(echo "$fpr" | tr -cd '[:xdigit:]')
        if [ ${#fpr} -lt 40 -o ${#fpr} -ne ${#b} ]; then
          echo "$pgm: '$fpr' does not look like a fingerprint" >&2
          continue
        fi
        [ $verbose = yes ] && echo "$pgm: refreshing $fpr ..." >&2
        if ! gpg $gpgopts --keyserver $keyserver --refresh-key $fpr ; then
          echo "$pgm: error refreshing '$fpr'" >&2
          continue
        fi
        [ $verbose = yes ] && echo "$pgm: Storing $fpr ..." >&2
        if ! gpg $gpgopts --export --yes --output "$htdocs/tmp-$fpr" $fpr ; then
           echo "$pgm: error exporting '$fpr'" >&2
           continue
        fi
        if cmp  "$htdocs/tmp-$fpr" "$htdocs/$fpr"; then
           rm "$htdocs/tmp-$fpr" || true
        else
           mv "$htdocs/tmp-$fpr" "$htdocs/$fpr"
           [ $verbose = yes ] && echo "$pgm: '$fpr' has been updated" >&2
        fi
    done
  )
fi

if [ $errors != no ]; then
    echo "$pgm: error encountered during processing" >&2
    exit 1
fi
exit 0
