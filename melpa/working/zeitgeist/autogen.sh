#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="zeitgeist-dataproviders"

(test -f $srcdir/configure.ac \
  && test -f $srcdir/autogen.sh) || {
    echo -n "**Error**: Directory \`$srcdir' does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

if which autoreconf ; then
  autoreconf --force --install || \
    (echo "There was an error in running autoreconf." > /dev/stderr;
     exit 1)
else
  echo "No build script available."
  echo " You need to install the following scripts:"
  echo "  * libtool"
  echo "  * automake"
  echo "  * autoconf"
  echo " Additionally, you need to make"
  echo " sure that they are in your \$PATH."
  exit 1
fi

echo Running $srcdir/configure $conf_flags "$@" ...
$srcdir/configure $conf_flags "$@" \
