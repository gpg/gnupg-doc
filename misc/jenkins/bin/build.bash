#!/bin/bash
# Script used jenkins to run builds for GnuPG and related packages.

# Stop on error and be nice to other processes.
set -xe
renice -n 10 -p $$

# Setup important envars
PREFIX=$HOME/prefix/$XTARGET
ORIGINAL_PREFIX=$HOME/prefix/$XTARGET
export PATH=/home/jenkins/bin:$PREFIX/bin:$PATH

# Print the environment.
env
ulimit -a
set +x
for f in /etc/gcrypt/hwf.deny /etc/gcrypt/fips_enabled ; do
  if [ -f "$f" ]; then
    echo "=== $f ==="
    cat -n "$f"
  fi
done
set -x

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

# Clean everything
git clean -fdx

# Run out autogen - note that --force is not required due to the git clean.
./autogen.sh

# Parallel jobs.
MAKEFLAGS="-j6"

# Parallel tests with our test suite.
export TESTFLAGS="--parallel"

SCANBUILD=
if [ "$(uname)" = Linux ] \
       && [ "$ROOT_BUILD_CAUSE_TIMERTRIGGER" = true ]; then
    # We only do scan-builds (which are really slow) on nightly
    # builds.
    SCANBUILD="scan-build -o ${WORKSPACE}/clangScanBuildReports -v"
fi
CONFIGUREFLAGS=
SANFLAGS=""
if [ "$(uname)" = Linux ]; then
    # XXX: We should check if the sanitizers are available.
    SANFLAGS="-fsanitize=undefined -fsanitize=address"
fi

if [ "$(uname)" = Darwin ]; then
    # XXX until we properly set this somewhere else
    cversion="_DARWIN_C_SOURCE=900000L"
    CFLAGS="$CFLAGS -D$cversion"
    CXXFLAGS="$CXXFLAGS -D$cversion"
fi

# Tweak the build depending on the package.
case "$JOB_NAME" in
    *tgpg*)
        MAKEFLAGS="$MAKEFLAGS GPG=/usr/bin/gpg2"
        ;;
    *gpgme*)
        # using libasan for python broke again, so disable the python
        # bindings for the sanitizer build
        if [ "$XTARGET" = sanitizer ]; then
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

# See if we have a GPGME checkout for the tesets.
xtest_gpgme_srcdir="$HOME/src/gpgme-for-gnupgs-tests"
if [ -d "$xtest_gpgme_srcdir/obj" ]; then
    export XTEST_GPGME_SRCDIR="$xtest_gpgme_srcdir"
    export XTEST_GPGME_BUILDDIR="$xtest_gpgme_srcdir/obj"
fi

# We build on the "obj" subdir.
abs_configure="$(pwd)/configure"
mkdir -p obj
cd obj

# Switch on the different targets.
case "$XTARGET" in
    native)
        ../configure --prefix=$PREFIX --enable-maintainer-mode \
	           $CONFIGUREFLAGS \
	           "$CONFIGUREFLAGS_0" \
	           CFLAGS="$CFLAGS -fPIC" \
	           CXXFLAGS="$CXXFLAGS -fPIC -std=c++11"
        make $MAKEFLAGS

        make -k check verbose=2 || true
        # Jenkins looks for "tests? failed" to mark a build unstable,
        # hence || true here

        make install
        ;;
    sanitizer)
	# asan breaks the configure tests, so we disable it here.
        ASAN_OPTIONS=detect_leaks=0 \
        $SCANBUILD \
            ../configure --prefix=$PREFIX --enable-maintainer-mode \
	           $CONFIGUREFLAGS \
	           "$CONFIGUREFLAGS_0" \
	           CFLAGS="$CFLAGS $SANFLAGS -fPIC" \
	           CXXFLAGS="$CXXFLAGS $SANFLAGS -fPIC -std=c++11"
        $SCANBUILD make $MAKEFLAGS

        make -k check verbose=2 || true
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
	# We need to point it to npth then...
	case "$JOB_NAME" in
	    gnupg/XTARGET=w32|gnupg-2.2/XTARGET=w32)
		CONFIGUREFLAGS="${CONFIGUREFLAGS} --with-npth-prefix=$ORIGINAL_PREFIX"
		;;
	    gnupg-2.0/XTARGET=w32)
		CONFIGUREFLAGS="${CONFIGUREFLAGS} --with-pth-prefix=$ORIGINAL_PREFIX --with-adns=$ORIGINAL_PREFIX"
		;;
	esac

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
			# We need to pass the absolute path of the iso.
			bash $HOME/bin/run-tests-w32.bash "$(readlink -f gnupg-test.iso)" || true
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

          # We use a different WORKDIR to avoid problems with too long
          # file names
	  cd "$WORKDIR"
          $abs_configure --prefix=$PREFIX --enable-maintainer-mode \
                   $CONFIGUREFLAGS
          make $MAKEFLAGS distcheck

          # Extract the tarname from the package
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
          # And do a final build using the generated tarball
	  cd ${tarname}
	  ./configure --prefix=$PREFIX $CONFIGUREFLAGS LD_LIBRARY_PATH=$PREFIX/lib
	  make $MAKEFLAGS
	  make $MAKEFLAGS install

        ;;
    *)
        echo "Bad XTARGET: '$XTARGET'"
        exit 2
esac
