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

echo "========================================================"
echo "gpgweb site building started on $(date -u -Iseconds)"
echo "========================================================"

emacs23 -q --batch  \
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

echo "========================================================="
echo "gpgweb site building finished on $(date -u -Iseconds)"
echo "========================================================="
