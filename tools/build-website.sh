#!/bin/sh

set -e

pgm=build-website.sh
root_dir="$(pwd)/gnupg-doc/web"
stage_dir="$(pwd)/gpgweb-stage"

if [ ! -d "${root_dir}" ]; then
   echo "$pgm: directory '${root_dir}' missing" >&2;
   exit 1
fi
if [ ! -d "${stage_dir}" ]; then
   echo "$pgm: directory '${stage_dir}' missing" >&2;
   exit 1
fi
cd "${root_dir}"

rev="$(git rev-parse --verify HEAD)"
if [ -z "$rev" ]; then
   echo "$pgm: No git revision found" >&2;
   exit 1
fi
revlast="$(head -1 ${stage_dir}/.revlast 2>/dev/null || true)"
if [ x"$rev" = x"$revlast" ]; then
   echo "$pgm: No need to build" >&2;
   exit 0
fi


echo "========================================================"
echo "gpgweb site building started on $(date -u -Iseconds)"
echo "========================================================"

emacs23 -q --batch  \
  --eval "(require 'assoc)" \
  --eval "(require 'org)" \
  --eval "(setq make-backup-files nil)" \
  --eval "(setq vc-handled-backends nil)" \
  --eval "(setq gpgweb-root-dir  \"${root_dir}/\")" \
  --eval "(setq gpgweb-stage-dir \"${stage_dir}/\")" \
  --eval "(require 'gpgweb (concat gpgweb-root-dir \"share/gpgweb.el\"))" \
  --eval "(setq org-publish-use-timestamps-flag nil)" \
  --eval "(setq org-export-html-toplevel-hlevel 1)" \
  --eval "(setq org-export-html-coding-system 'utf-8)" \
  --eval "(gpgweb-setup-project)" \
  --eval "(org-publish-initialize-cache \"gpgweb\")" \
  --eval "(setq debug-on-error nil)" \
  --eval "(org-publish \"gpgweb\" t nil)"

echo "$rev" > ${stage_dir}/.revlast

echo "========================================================="
echo "gpgweb site building finished on $(date -u -Iseconds)"
echo "========================================================="
