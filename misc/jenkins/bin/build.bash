#!/bin/bash

set -xe

renice -n 10 -p $$

PREFIX=$HOME/prefix/$XTARGET
ORIGINAL_PREFIX=$HOME/prefix/$XTARGET
export PATH=$PREFIX/bin:$PATH

env

# Tweak the prefix we're installing this project into.  For gnupg-1.4
# and friends.
case "$JOB_NAME" in
    *-1.4*)
        PREFIX=$PREFIX-1.4
        ;;
    *-2.0*)
        PREFIX=$PREFIX-2.0
        ;;
    *-2.2*)
        PREFIX=$PREFIX-2.2
        ;;
esac
mkdir -p $PREFIX

fix_permissions()
{
  find $1 -type d -exec chmod +w {} + || true
}

fix_permissions .
git clean -fdx
./autogen.sh

MAKEFLAGS="-j2"
SCANBUILD=
if [ "$(uname)" = Linux ]; then
    # XXX: We should really have an analyzer target
    SCANBUILD="scan-build -o ${WORKSPACE}/clangScanBuildReports -v"
fi
CONFIGUREFLAGS=
SANFLAGS=""
if [ "$(uname)" = Linux ]; then
    # XXX: We should really have an analyzer target
    SANFLAGS="-fsanitize=undefined -fsanitize=address"
fi

if [ "$(uname)" = Darwin ]; then
    # XXX until we properly set this somewhere else
    cversion="_DARWIN_C_SOURCE=900000L"
    CFLAGS="$CFLAGS -D$cversion"
    CXXFLAGS="$CXXFLAGS -D$cversion"
fi

case "$JOB_NAME" in
    *tgpg*)
        MAKEFLAGS="$MAKEFLAGS GPG=/usr/bin/gpg2"
        ;;
    *gpgme*)
        # using libasan for python broke again, so disable the python bindings for the native build
        if [ "$XTARGET" = native ] && [ "$label" != macos ]; then
            CONFIGUREFLAGS_0="--enable-languages=cpp qt"
        fi
        ;;
    *gnupg*)
        if [ "$XTARGET" = native ]; then
            CONFIGUREFLAGS="--enable-wks-tools --enable-g13 --enable-symcryptrun --enable-gpg2-is-gpg"
        fi
        if [ "$XTARGET" = w32 ]; then
            CONFIGUREFLAGS="--enable-wks-tools --enable-gpg2-is-gpg --with-zlib=$ORIGINAL_PREFIX --with-bzip2=$ORIGINAL_PREFIX"
        fi
	if [ "$NODE_NAME" = zygalski ]; then
	    CONFIGUREFLAGS="$CONFIGUREFLAGS --with-libiconv-prefix=$HOME/pkg"
	fi
        ;;
esac

abs_configure="$(pwd)/configure"
mkdir -p obj
cd obj

case "$XTARGET" in
    native)
        ASAN_OPTIONS=detect_leaks=0 \
        $SCANBUILD \
            ../configure --prefix=$PREFIX --enable-maintainer-mode \
	           $CONFIGUREFLAGS \
	           "$CONFIGUREFLAGS_0" \
	           CFLAGS="$CFLAGS $SANFLAGS -fPIC" \
	           CXXFLAGS="$CXXFLAGS $SANFLAGS -fPIC -std=c++11"
        $SCANBUILD make $MAKEFLAGS

        # so make sure the asan runtime is there for e.g. python
        PATH=/home/jenkins/bin:$PATH \
        LD_LIBRARY_PATH=$PREFIX/lib \
          make check verbose=2 LD_LIBRARY_PATH=$PREFIX/lib || true
        # Jenkins looks for "tests? failed" to mark a build unstable,
        # hence || true here

        make install
        ;;
    w32)
        export w32root=$PREFIX

	# autogen.rc adds --with-gpg-error-prefix=@SYSROOT@, so we cannot
	# install to a prefix that doesn't also contain all the dependencies,
	# patch that out, so that the gpg-error-config and friends are located
	# using PATH
	if [ -f "/home/jenkins/bin/$(dirname $JOB_NAME)-w32.patch" ]; then
	  ( cd .. && patch -p1 <"/home/jenkins/bin/$(dirname $JOB_NAME)-w32.patch" )
	fi
	# We need to point it to npth and adns then...
	CONFIGUREFLAGS="${CONFIGUREFLAGS} --with-npth-prefix=$ORIGINAL_PREFIX --with-adns=$ORIGINAL_PREFIX"

        # gpg1's autogen.sh does not add --enable-maintainer-mode, so
        # version.texi is not generated.  we add it here to be sure.
        # likewise for --prefix
        ../autogen.sh --build-w32 --enable-maintainer-mode --prefix=$PREFIX \
          $CONFIGUREFLAGS
        make $MAKEFLAGS
        make install

	case "$JOB_NAME" in
		gnupg/*|gnupg-2.2/*)
			bash /home/jenkins/bin/make-windows-cd.sh
			;;
	esac
        ;;
    distcheck)
        CONFIGUREFLAGS=
	WORKDIR="$(mktemp -d)"
	cleanup()
	{
	  cd /tmp
	  fix_permissions "$WORKDIR"
	  rm -rf -- "$WORKDIR" || true
	}
	trap cleanup EXIT

	  cd "$WORKDIR"
          $abs_configure --prefix=$PREFIX --enable-maintainer-mode \
                   $CONFIGUREFLAGS LD_LIBRARY_PATH=$PREFIX/lib
          make $MAKEFLAGS distcheck
          tarname=$(awk <config.h '
	             /^#define PACKAGE_TARNAME/ {gsub(/"/,"",$3);name=$3};
	             /^#define PACKAGE_VERSION/ {gsub(/"/,"",$3);vers=$3};
		     END {print name "-" vers}')
	  if [ -f "${tarname}.tar.bz2" ]; then
	     bzcat "${tarname}.tar.bz2" | tar xf -
	  elif [ -f "${tarname}.tar.gz" ]; then
	     zcat "${tarname}.tar.gz" | tar xf -
          else
	     echo "No tarball named $tarname found - skipping installation" >&2
	     exit 0
	  fi
	  cd ${tarname}
	  ./configure --prefix=$PREFIX $CONFIGUREFLAGS LD_LIBRARY_PATH=$PREFIX/lib
	  make $MAKEFLAGS
	  make $MAKEFLAGS install

        ;;
    *)
        echo "Bad XTARGET: '$XTARGET'"
        exit 2
esac
