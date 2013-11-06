#! /bin/sh
# Run this to generate all the initial makefiles, etc.
#
# Copyright (C) 2003 g10 Code GmbH
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#
# Check the git setup.
#
if [ -d .git ]; then
  if [ -f .git/hooks/pre-commit.sample -a ! -f .git/hooks/pre-commit ] ; then
    cat <<EOF >&2
*** Activating trailing whitespace git pre-commit hook. ***
    For more information see this thread:
      http://mail.gnome.org/archives/desktop-devel-list/2009-May/msg00084html
    To deactivate this pre-commit hook again move .git/hooks/pre-commit
    and .git/hooks/pre-commit.sample out of the way.
EOF
      cp -av .git/hooks/pre-commit.sample .git/hooks/pre-commit
      chmod +x  .git/hooks/pre-commit
  fi
  if [ -f build-aux/git-hooks/commit-msg -a ! -f .git/hooks/commit-msg ] ; then
    cat <<EOF >&2
*** Activating commit log message check hook. ***
EOF
      cp -av build-aux/git-hooks/commit-msg .git/hooks/commit-msg
      chmod +x  .git/hooks/commit-msg
  fi
fi
