#!/bin/bash
# Script used jenkins to run builds for GnuPG and related packages.

# Stop on error and be nice to other processes.
set -xe
renice -n 10 -p $$

# Configuration.
MAKE=make

XTARGET="${XTARGET:-native}"

# Platform-specific configuration.
case "$(uname)" in
    OpenBSD)
	MAKE=gmake
	;;
esac

if [ "$XTARGET" = w32 ]; then
    CC=i686-w64-mingw32-gcc
    CXX=i686-w64-mingw32-g++
fi

# Setup ccache if installed.
if ccache --version >/dev/null; then
    export CCACHE_DIR="$HOME/cache/$JOB_NAME"
    mkdir -p "$CCACHE_DIR"
    export CC="ccache ${CC:-gcc}"
    export CXX="ccache ${CXX:-g++}"
fi

# Setup important envars
PREFIX=$HOME/prefix/$XTARGET
ORIGINAL_PREFIX=$HOME/prefix/$XTARGET

# hackhackhack
#
# Copy all *-config scripts into a separate directory and put that
# into PATH.  We want configure to pick them up, but we do not
# necessarily want to use all the other tools from $PREFIX/bin,
# because then we would have to point LD_LIBRARY_PATH to $PREFIX/lib,
# which we want to avoid at all costs.
mkdir -p $PREFIX/bin-config
cp $PREFIX/bin/*-config $PREFIX/bin-config
export PATH=$PREFIX/bin-config:$PATH
# kcahkcahkcah

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

	# Disable Python bindings on macOS.  Something is not working
	# there.
        if [ "$NODE_NAME" = zygalski ]; then
            CONFIGUREFLAGS_0="--enable-languages=cpp qt"
        fi
        ;;
    *gnupg*)
	# Common configure options.
	CONFIGUREFLAGS="--enable-wks-tools --enable-gpg2-is-gpg"

	# For Windows builds...
        if [ "$XTARGET" = w32 ]; then
	    # ... we need to tweak it a little and we leave out some
	    # stuff...
            CONFIGUREFLAGS="$CONFIGUREFLAGS --with-zlib=$ORIGINAL_PREFIX --with-bzip2=$ORIGINAL_PREFIX"
	else
	    # ... that we enable for all other builds.
	    CONFIGUREFLAGS="$CONFIGUREFLAGS --enable-g13 --enable-symcryptrun"
        fi

	if [ "$NODE_NAME" = zygalski ]; then
	    CONFIGUREFLAGS="$CONFIGUREFLAGS --with-libiconv-prefix=$HOME/pkg"
	fi
	if [ "$NODE_NAME" = openbsd60 ]; then
	    CONFIGUREFLAGS="$CONFIGUREFLAGS --with-libiconv-prefix=$HOME/compat --with-bzip2=$HOME/compat"
	fi

	# Disable NTBTLS for now until it is actually mature and used.
	CONFIGUREFLAGS="$CONFIGUREFLAGS --disable-ntbtls"
        ;;
esac

# See if we have a GPGME checkout for the tesets.
xtest_gpgme_srcdir="$HOME/src/gpgme-for-gnupgs-tests"
if [ -d "$xtest_gpgme_srcdir/obj-$XTARGET" ]; then
    # Some targets, like the sanitizer target, require a custom
    # version of GPGME.
    export XTEST_GPGME_SRCDIR="$xtest_gpgme_srcdir"
    export XTEST_GPGME_BUILDDIR="$xtest_gpgme_srcdir/obj-$XTARGET"
elif [ -d "$xtest_gpgme_srcdir/obj" ]; then
    export XTEST_GPGME_SRCDIR="$xtest_gpgme_srcdir"
    export XTEST_GPGME_BUILDDIR="$xtest_gpgme_srcdir/obj"
fi

# The libraries use RUNPATH when linking the tests, so they locate
# their dependencies that way.  GnuPG, however, does not.  Therefore,
# we set LD_LIBRARY_PATH.
test_environment="LD_LIBRARY_PATH=$ORIGINAL_PREFIX/lib"

# HACKHACKHACK:
#
# Because newer Debian toolchains prefer RUNPATH over RPATH, and
# RUNPATH has lower precedence than LD_LIBRARY_PATH, we need to
# explicitly add libtool's .libs directory:
case "$JOB_NAME" in
  *gnupg*)
    if [ "${XTEST_GPGME_BUILDDIR}" ]; then
	test_environment="LD_LIBRARY_PATH=${XTEST_GPGME_BUILDDIR}/src/.libs:${XTEST_GPGME_BUILDDIR}/lang/cpp/src/.libs:${XTEST_GPGME_BUILDDIR}/lang/qt/src/.libs:$ORIGINAL_PREFIX/lib"
    fi
    ;;
  *gpgme*)
    test_environment="LD_LIBRARY_PATH=$(pwd)/obj/src/.libs:$(pwd)/obj/lang/cpp/src/.libs:$(pwd)/obj/lang/qt/src/.libs:$ORIGINAL_PREFIX/lib"
    ;;
  *)
    test_environment="LD_LIBRARY_PATH=$(pwd)/obj/src/.libs:$ORIGINAL_PREFIX/lib"
    ;;
esac
#
# If we don't do this, the version tests fail because the runtime
# linker will pick up the library from LD_LIBRARY_PATH.  Also, testing
# the installed version is not what we want ofc.
#
# KCAHKCAHKCAH

# We build on the "obj" subdir.
abs_configure="$(pwd)/configure"
mkdir -p obj
cd obj


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


# Switch on the different targets.
case "$XTARGET" in
    native)
        ../configure --prefix=$PREFIX --enable-maintainer-mode \
	           $CONFIGUREFLAGS \
	           "$CONFIGUREFLAGS_0" \
		   CFLAGS="$CFLAGS" \
	           CXXFLAGS="$CXXFLAGS -std=c++11"
        $MAKE $MAKEFLAGS

        env $test_environment $MAKE -k check verbose=2 \
	    || echo "FAIL: make check failed with status $?"
        # Jenkins looks for "FAIL:" to mark a build unstable,
        # hence || ... here

        $MAKE install
        ;;
    in-tree)
	cd ..
        ./configure --prefix=$PREFIX --enable-maintainer-mode \
	           $CONFIGUREFLAGS \
	           "$CONFIGUREFLAGS_0" \
		   CFLAGS="$CFLAGS" \
	           CXXFLAGS="$CXXFLAGS -std=c++11"
        $MAKE $MAKEFLAGS

	# HACKHACKHACK: Fix the test_environment hack.
	test_environment="$(echo $test_environment | sed -e 's#obj/##g')"
	# KCAHKCAHKCAH

        env $test_environment $MAKE -k check verbose=2 \
	    || echo "FAIL: make check failed with status $?"
        # Jenkins looks for "FAIL:" to mark a build unstable,
        # hence || ... here

        $MAKE install
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
        $SCANBUILD $MAKE $MAKEFLAGS

        env $test_environment $MAKE -k check verbose=2 \
	    || echo "FAIL: make check failed with status $?"
        # Jenkins looks for "FAIL:" to mark a build unstable,
        # hence || ... here

        $MAKE install
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
        $MAKE $MAKEFLAGS
        $MAKE install

	case "$JOB_NAME" in
		gnupg/*|gnupg-2.2/*)
			bash /home/jenkins/bin/make-windows-cd.sh
			# We need to pass the absolute path of the iso.
			bash $HOME/bin/run-tests-w32.bash "$(readlink -f gnupg-test.iso)" || echo "Warning: error running tests on Windows."
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

	  # Extract the directory / tarname from the package
          tarname=$(awk <config.h '
	             /^#define PACKAGE_TARNAME/ {gsub(/"/,"",$3);name=$3};
	             /^#define PACKAGE_VERSION/ {gsub(/"/,"",$3);vers=$3};
		     END {print name "-" vers}')

	  # HACKHACKHACK: Because newer Debian toolchains prefer
	  # RUNPATH over RPATH, and RUNPATH has lower precedence than
	  # LD_LIBRARY_PATH, we need to explicitly add libtool's .libs
	  # directory:
	  case "$JOB_NAME" in
	    *gnupg*)
	      if [ "${XTEST_GPGME_BUILDDIR}" ]; then
		  test_environment="LD_LIBRARY_PATH=${XTEST_GPGME_BUILDDIR}/src/.libs:${XTEST_GPGME_BUILDDIR}/lang/cpp/src/.libs:${XTEST_GPGME_BUILDDIR}/lang/qt/src/.libs:$ORIGINAL_PREFIX/lib"
	      fi
	      ;;
	    *gpgme*)
	      test_environment="LD_LIBRARY_PATH=$(pwd)/${tarname}/_build/sub/src/.libs:$(pwd)/${tarname}/_build/sub/lang/cpp/src/.libs:$(pwd)/${tarname}/_build/sub/lang/qt/src/.libs:$ORIGINAL_PREFIX/lib"
	      ;;
	    *)
	      test_environment="LD_LIBRARY_PATH=$(pwd)/${tarname}/_build/sub/src/.libs:$ORIGINAL_PREFIX/lib"
	      ;;
	  esac
	  # KCAHKCAHKCAH

	  if ! env $test_environment $MAKE $MAKEFLAGS distcheck ; then
              # Jenkins looks for "FAIL:" to mark a build unstable,
              # hence we ignore errors here.
	      echo "FAIL: make distcheck failed with status $?"
	      # Disable the cleanup so that we can investigate.
	      trap - EXIT
	      exit 0
	  fi

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
	  ./configure --prefix=$PREFIX $CONFIGUREFLAGS
	  $MAKE $MAKEFLAGS
	  $MAKE $MAKEFLAGS install

        ;;
    *)
        echo "Bad XTARGET: '$XTARGET'"
        exit 2
esac
