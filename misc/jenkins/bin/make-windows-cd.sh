#!/bin/sh

set -ex

if ! [ -f config.log ] || ! grep -q mingw config.log; then
    echo "must be run from a configured windows build environment"
fi

[ -z "$w32root" ] && w32root="$HOME/w32root"
ADDITIONAL_FILES=
IMAGE=gnupg-test.iso
XTEST_GPGME_SRCDIR=$HOME/src/gpgme-for-gnupgs-tests
XTEST_GPGME_BUILDDIR=$HOME/src/gpgme-for-gnupgs-tests/obj.w32

[ -f make-windows-cd.rc ] && . make-windows-cd.rc

# we pick binaries from the prefix, so make sure they are current.
make install

WORKDIR="$(mktemp --directory)"
TARGET="${WORKDIR}/gnupg"

mkdir "$TARGET"

[ "$ADDITIONAL_FILES" ] && cp -v $(ls -1 $ADDITIONAL_FILES) $TARGET
cp -v $w32root/bin/*.exe $w32root/bin/*.dll $TARGET
cp -v tests/gpgscm/*.exe $TARGET
# XXX mk-tdata is on the way out
cp -v tools/mk-tdata.exe $TARGET || true
cp -v agent/gpg-preset-passphrase.exe $TARGET
cp -v -a ../tests $TARGET
if [ -e "$XTEST_GPGME_SRCDIR" ] && [ -e "$XTEST_GPGME_BUILDDIR" ]; then
    cp -a "$XTEST_GPGME_SRCDIR" $TARGET/gpgme
    cp -v "$XTEST_GPGME_BUILDDIR"/src/.libs/*.exe $TARGET
    cp -v "$XTEST_GPGME_BUILDDIR"/src/.libs/*.dll $TARGET
    # Strip .git.
    rm -rf -- $TARGET/gpgme/.git
    # Remove native build if it exists.
    rm -rf -- $TARGET/gpgme/obj
fi
cp -v -a ../tests $TARGET
cp -v tests/openpgp/fake-pinentry.exe $TARGET
cp -v /home/jenkins/bin/run-tests.bat $WORKDIR
[ -f "$IMAGE" ] && rm -f "$IMAGE"
genisoimage --output "$IMAGE" -J "$WORKDIR"
[ "${WORKDIR}" ] && rm -rf -- "${WORKDIR}"
