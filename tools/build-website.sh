#!/bin/sh
# Build the gnupg.org website from a git working directory.
# This script requires two users
#   webbuilder - the user to run this script
#   webbuild-x - the user used by this script to run emacs
# A certain directory layout is required with permissions setup
# so that the webbuild-x has only write access to the stage area
# and to its own home directory.  The scripts checks the permissions.
#
# These cronjobs are required:
# --8<---------------cut here---------------start------------->8---
# # Pull the master branch of the web pages
# */20  * * * * cd /home/webbuilder/gnupg-doc && git pull -q origin master
#
# # In case of race conditions we try to build every few ours again.
# 35  */7 * * * /home/webbuilder/bin/build-website.sh --cron
# --8<---------------cut here---------------end--------------->8---
#

set -e

pgm=build-website.sh
mainuser=webbuilder
workuser=webbuild-x

# We use a fixed HOME so that this script can be run here from other
# accounts.
HOME=$(awk </etc/passwd -F: '$1=="'$mainuser'" {print $6;exit}')
if [ ! -d "$HOME" ]; then
   echo "$pgm: directory '${HOME}' missing" >&2;
   exit 1
fi

reponame=gnupg-doc

workuser_dir=$HOME/${workuser}
log_dir="$HOME/log"
root_dir="$HOME/${reponame}/web"
stage_dir="$HOME/gpgweb-stage"
LOCKFILE="${log_dir}/${reponame}.lock"

if [ x"$1" = x"--git" ]; then
  shift
  exec  >>${log_dir}/"$reponame".log 2>&1
  echo "$(date -u -Iseconds) gpgweb site build was git triggered"
elif [ x"$1" = x"--cron" ]; then
  shift
  exec  >>${log_dir}/"$reponame".log 2>&1
  echo "$(date -u -Iseconds) gpgweb site build was cron triggered"
fi

if ! id $workuser >/dev/null 2>&1 ; then
   echo "$pgm: sudo user '${workuser}' not available" >&2;
   exit 1
fi

# Check directories
for f in "${workuser_dir}" "${root_dir}" "${stage_dir}"; do
  if [ ! -d "$f" ]; then
     echo "$pgm: directory '$f' missing" >&2;
     exit 1
  fi
done
want="2775:${workuser}:${mainuser}"
for f in "${workuser_dir}" "${stage_dir}"; do
  x=$(stat -c '%a:%U:%G' "$f")
  if [ x"$x" != x"$want" ]; then
    echo "$pgm: directory '$f' has wrong permissions" >&2
    echo "$pgm:   want: $want" >&2
    echo "$pgm:   have: $x" >&2
    exit 1
  fi
done

# Take a lock
if ! lockfile -l 7200 -r 2 $LOCKFILE; then
    echo "$pgm: another instance is still running" >&2
    exit 0
fi
trap "rm -f $LOCKFILE" 0

cd "${root_dir}"

rev="$(git rev-parse --verify HEAD)"
if [ -z "$rev" ]; then
   echo "$pgm: No git revision found" >&2;
   exit 1
fi
revlast="$(head -1 ${log_dir}/${reponame}.revlast 2>/dev/null || true)"
if [ x"$rev" = x"$revlast" ]; then
   echo "$pgm: No need to build" >&2;
   exit 0
fi


echo "$(date -u -Iseconds) gpgweb site build started"
echo "=================================================="

sudo -u webbuild-x emacs24 -q --batch  \
  --eval "(require 'assoc)" \
  --eval "(require 'org)" \
  --eval "(setq make-backup-files nil)" \
  --eval "(setq gpgweb-root-dir  \"${root_dir}/\")" \
  --eval "(setq gpgweb-stage-dir \"${stage_dir}/\")" \
  --eval "(require 'gpgweb (concat gpgweb-root-dir \"share/gpgweb.el\"))" \
  --eval "(setq org-publish-use-timestamps-flag nil)" \
  --eval "(setq org-export-html-toplevel-hlevel 1)" \
  --eval "(setq org-export-html-coding-system 'utf-8)" \
  --eval "(gpgweb-setup-project)" \
  --eval "(org-publish-initialize-cache \"gpgweb\")" \
  --eval "(message \"root=(%s)\" gpgweb-root-dir)" \
  --eval "(org-publish \"gpgweb\" t nil)"

echo "$rev" > ${log_dir}/${reponame}.revlast

echo "==================================================="
echo "$(date -u -Iseconds) gpgweb site build finished"
