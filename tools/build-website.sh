#!/bin/sh
# Build the gnupg.org website from a git working directory.
#
# This script requires two users
#
#   webbuilder - the user to run this script
#   webbuild-x - the user used by this script to run emacs
#   webbuild-y - the user used by this script to run emacs (for preview)
#
# A certain directory layout is required with permissions setup
# so that the webbuild-x has only write access to the stage area
# and to its own home directory.  The script checks the permissions.
#
# The trigger-website-build scripts is expected to be installed
# as git post-merge hook.
#
# These cronjobs are required for user webbuilder:
# --8<---------------cut here---------------start------------->8---
# # Pull the master branch of the web pages
# */20  * * * * cd /home/webbuilder/gnupg-doc && git pull -q origin master
# */18  * * * * cd /home/webbuilder/gnupg-doc-preview && git pull -q origin preview
#
# # In case of race conditions we try to build every few ours again.
# 35  */7 * * * /home/webbuilder/bin/build-website.sh --cron
# --8<---------------cut here---------------end--------------->8---
#
# /etc/sudoers needs this:
# --8<---------------cut here---------------start------------->8---
# # Let webbuilder run any command as user webbuild-x
# webbuilder         ALL = (webbuild-x,webbuild-y) NOPASSWD: ALL
# --8<---------------cut here---------------end--------------->8---
#

set -e

pgm=build-website.sh
mainuser=webbuilder
workuser=webbuild-x
workuser_pv=webbuild-y

# We use a fixed HOME so that this script can be run here from other
# accounts.
HOME=$(awk </etc/passwd -F: '$1=="'$mainuser'" {print $6;exit}')
if [ ! -d "$HOME" ]; then
   echo "$pgm: directory '${HOME}' missing" >&2;
   exit 1
fi

reponame=gnupg-doc
htdocs_web="/var/www/www/www.gnupg.org/htdocs"
htdocs_preview="/var/www/www/preview.gnupg.org/htdocs"
htdocs_blog="/var/www/www/www.gnupg.org/misc/blog"

workuser_dir=$HOME/${workuser}
workuser_pv_dir=$HOME/${workuser_pv}
log_dir="$HOME/log"
root_dir="$HOME/${reponame}"
root_dir_pv="$HOME/${reponame}-preview"
stage_dir="$HOME/${reponame}-stage"
stage_dir_pv="$HOME/${reponame}-preview-stage"
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

if ! id $workuser_pv >/dev/null 2>&1 ; then
   echo "$pgm: sudo user '${workuser_pv}' not available" >&2;
   exit 1
fi

# Check directories
for f in "${workuser_dir}" "${root_dir}" "${stage_dir}" \
         "${workuser_pv_dir}" "${root_dir_pv}" "${stage_dir_pv}"; do
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
want="2775:${workuser_pv}:${mainuser}"
for f in "${workuser_pv_dir}" "${stage_dir_pv}"; do
  x=$(stat -c '%a:%U:%G' "$f")
  if [ x"$x" != x"$want" ]; then
    echo "$pgm: directory '$f' has wrong permissions" >&2
    echo "$pgm:   want: $want" >&2
    echo "$pgm:   have: $x" >&2
    exit 1
  fi
done

cd "${root_dir}"

#
# Take a lock so that only one instance of this script runs.
#
if ! lockfile -l 7200 -r 2 $LOCKFILE; then
    echo "$pgm: another instance is still running" >&2
    exit 0
fi
trap "rm -f $LOCKFILE" 0


# These flags are set to the stage directory if a sync is required
sync_web=
sync_blog=
sync_preview=


#
# Build main part
#
subdir=web

revlastfile="${log_dir}/${reponame}.$(echo $subdir | tr / _).revlast"
buildlog="${log_dir}/${reponame}.$(echo $subdir | tr / _).log"
rev="$(git rev-parse --verify HEAD:$subdir)"
if [ -z "$rev" ]; then
   echo "$pgm: No git revision found" >&2;
   exit 1
fi
revlast="$(head -1 ${revlastfile} 2>/dev/null || true)"
if [ x"$rev" = x"$revlast" ]; then
   echo "$pgm: No need to build $subdir" >&2;
else

  echo "$(date -u -Iseconds) build started for $subdir" | tee ${buildlog}

  if [ ! -d ${stage_dir}/${subdir} ]; then
      sudo -u webbuild-x mkdir ${stage_dir}/${subdir}
  fi

  sudo 2>>${buildlog} -u webbuild-x emacs24 -q --batch  \
  --eval "(require 'assoc)" \
  --eval "(require 'org)" \
  --eval "(setq make-backup-files nil)" \
  --eval "(setq gpgweb-root-dir  \"${root_dir}/${subdir}/\")" \
  --eval "(setq gpgweb-stage-dir \"${stage_dir}/${subdir}/\")" \
  --eval "(require 'gpgweb (concat gpgweb-root-dir \"share/gpgweb.el\"))" \
  --eval "(setq org-publish-use-timestamps-flag nil)" \
  --eval "(setq org-export-html-toplevel-hlevel 1)" \
  --eval "(setq org-export-html-coding-system 'utf-8)" \
  --eval "(gpgweb-setup-project)" \
  --eval "(org-publish-initialize-cache \"gpgweb\")" \
  --eval "(message \"root=(%s)\" gpgweb-root-dir)" \
  --eval "(org-publish \"gpgweb\" t nil)"

  echo "$rev" > ${revlastfile}
  sync_web=${stage_dir}/${subdir}
  echo "$(date -u -Iseconds) build finished for $subdir" | tee -a ${buildlog}
fi


#
# Build blogs
#
subdir=misc/blog.gnupg.org

revlastfile="${log_dir}/${reponame}.$(echo $subdir | tr / _).revlast"
buildlog="${log_dir}/${reponame}.$(echo $subdir | tr / _).log"
rev="$(git rev-parse --verify HEAD:$subdir)"
if [ -z "$rev" ]; then
   echo "$pgm: No git revision found" >&2;
   exit 1
fi
revlast="$(head -1 ${revlastfile} 2>/dev/null || true)"
if [ x"$rev" = x"$revlast" ]; then
   echo "$pgm: No need to build $subdir" >&2;
else

  echo "$(date -u -Iseconds) build started for $subdir" | tee ${buildlog}

  if [ ! -d ${stage_dir}/${subdir} ]; then
      sudo -u webbuild-x mkdir -p ${stage_dir}/${subdir}
  fi
  cd ${stage_dir}/${subdir}

  # We need to initialize that org cache to use our own publish function
  # despite that we do not use any org-publish feature
  echo "$pgm: Rendering blogs" >&2
  sudo 2>>${buildlog} -u webbuild-x emacs24 -q --batch \
  --eval "(require 'assoc)" \
  --eval "(require 'org)" \
  --eval "(setq gpgweb-root-dir \"${root_dir}/web/\")" \
  --eval "(setq gpgweb-blog-dir \"${root_dir}/${subdir}/\")" \
  --eval "(setq gpgweb-stage-dir \"${stage_dir}/${subdir}/\")" \
  --eval "(require 'gpgweb (concat gpgweb-root-dir \"share/gpgweb.el\"))" \
  --eval "(setq org-publish-use-timestamps-flag nil)" \
  --eval "(setq org-export-html-toplevel-hlevel 1)" \
  --eval "(setq org-export-html-coding-system 'utf-8)" \
  --eval "(gpgweb-setup-project)" \
  --eval "(org-publish-initialize-cache \"gpgweb\")" \
  --eval "(message \"root=(%s)\" gpgweb-root-dir)" \
  --eval "(gpgweb-publish-blogs)"

  echo "$pgm: Updating blog index" >&2
  indexcreator="${root_dir}/${subdir}/update-index.sh"
  if [ ! -f $indexcreator ]; then
    echo "$pgm: $indexcreator not found" >&2
    exit 1
  fi
  sudo -u webbuild-x ${indexcreator}

  echo "$rev" > ${revlastfile}
  sync_blog=${stage_dir}/${subdir}
  echo "$(date -u -Iseconds) build finished for $subdir" | tee -a ${buildlog}

fi


#
# Build the preview site (w/o blogs)
#
branch=preview
subdir=web

cd "${root_dir_pv}"

revlastfile="${log_dir}/${reponame}.$branch.$(echo $subdir | tr / _).revlast"
buildlog="${log_dir}/${reponame}.$branch.$(echo $subdir | tr / _).log"
rev="$(git rev-parse --verify $branch:$subdir)"
if [ -z "$rev" ]; then
   echo "$pgm: No git revision found" >&2;
   exit 1
fi
revlast="$(head -1 ${revlastfile} 2>/dev/null || true)"
if [ x"$rev" = x"$revlast" ]; then
   echo "$pgm: No need to build $branch:$subdir" >&2;
else

  echo "$(date -u -Iseconds) build started for $branch:$subdir" | tee ${buildlog}

  if [ ! -d ${stage_dir_pv}/${subdir} ]; then
      sudo -u webbuild-y mkdir ${stage_dir_pv}/${subdir}
  fi

  sudo 2>>${buildlog} -u webbuild-y emacs24 -q --batch  \
  --eval "(require 'assoc)" \
  --eval "(require 'org)" \
  --eval "(setq make-backup-files nil)" \
  --eval "(setq gpgweb-root-dir  \"${root_dir_pv}/${subdir}/\")" \
  --eval "(setq gpgweb-stage-dir \"${stage_dir_pv}/${subdir}/\")" \
  --eval "(require 'gpgweb (concat gpgweb-root-dir \"share/gpgweb.el\"))" \
  --eval "(setq org-publish-use-timestamps-flag nil)" \
  --eval "(setq org-export-html-toplevel-hlevel 1)" \
  --eval "(setq org-export-html-coding-system 'utf-8)" \
  --eval "(gpgweb-setup-project)" \
  --eval "(org-publish-initialize-cache \"gpgweb\")" \
  --eval "(message \"root=(%s)\" gpgweb-root-dir)" \
  --eval "(org-publish \"gpgweb\" t nil)"

  echo "$rev" > ${revlastfile}
  sync_preview=${stage_dir_pv}/${subdir}
  echo "$(date -u -Iseconds) build finished for $branch:$subdir" | tee -a ${buildlog}
fi
cd "${root_dir}"


#
# Sync to the webspace
#
cd "${root_dir}"
any_sync=

if [ -n "$sync_web" ]; then
  cd "$sync_web"
  rsync -rlt --exclude '*~' --exclude '*.tmp' \
        . ${htdocs_web}/
  touch ${htdocs_web}/donate/donors.dat
  cd ${htdocs_web}
  ln -sf ../../howtos.gnupg.org/htdocs howtos
  ln -sf software related_software
  ln -sf software/index.html features.html
  cd "$sync_web"
  any_sync=yes
fi

if [ -n "$sync_blog" ]; then
  cd "$sync_blog"
  rsync -rt --links --exclude '*~' --exclude '*.sh' \
        --exclude '*tmp' --exclude '*.org' . ${htdocs_blog}/
  cd "$root_dir/misc/blog.gnupg.org"
  rsync -rt --links --exclude '*~' --exclude '*.sh' \
        --exclude '*tmp' --exclude '*.org' img data  ${htdocs_blog}/
  any_sync=yes
fi

if [ -n "$sync_preview" ]; then
  cd "$sync_preview"
  rsync -rlt --exclude '*~' --exclude '*.tmp' \
        . ${htdocs_preview}/
  $HOME/bin/mkkudos.sh --verbose --force --test
fi


cd "${root_dir}"

if [ "$any_sync" = yes ]; then
  $HOME/bin/mkkudos.sh --verbose --force
fi


#
# Print warnings when the scripts are out of date
# (For security reasons the scripts need to be installed manually.)
#
for f in trigger-website-build build-website.sh mkkudos.sh \
         append-to-donors.sh ; do
  if ! cmp -s ${HOME}/bin/$f tools/$f ; then
    echo "$pgm: Warning: A newer version of $f is available" >&2;
  fi
done

exit 0
