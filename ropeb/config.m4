dnl $Id$
dnl config.m4 for extension ropeb

dnl Comments in this file start with the string 'dnl'.
dnl Remove where necessary. This file will not work
dnl without editing.

dnl If your extension references something external, use with:

dnl PHP_ARG_WITH(ropeb, for ropeb support,
dnl Make sure that the comment is aligned:
dnl [  --with-ropeb             Include ropeb support])

dnl Otherwise use enable:

PHP_ARG_ENABLE(ropeb, whether to enable php-erlang bridge support,
[  --enable-ropeb           Enable php-erlang bridge support])

if test "$PHP_ROPEB" = "yes"; then
  dnl Write more examples of tests here...

  dnl # --with-ropeb -> check with-path
  dnl SEARCH_PATH="/usr/local /usr"     # you might want to change this
  dnl SEARCH_FOR="/include/ropeb.h"  # you most likely want to change this
  dnl if test -r $PHP_ROPEB/$SEARCH_FOR; then # path given as parameter
  dnl   ROPEB_DIR=$PHP_ROPEB
  dnl else # search default path list
  dnl   AC_MSG_CHECKING([for ropeb files in default path])
  dnl   for i in $SEARCH_PATH ; do
  dnl     if test -r $i/$SEARCH_FOR; then
  dnl       ROPEB_DIR=$i
  dnl       AC_MSG_RESULT(found in $i)
  dnl     fi
  dnl   done
  dnl fi
  dnl
  dnl if test -z "$ROPEB_DIR"; then
  dnl   AC_MSG_RESULT([not found])
  dnl   AC_MSG_ERROR([Please reinstall the ropeb distribution])
  dnl fi

  dnl # --with-ropeb -> add include path
  dnl PHP_ADD_INCLUDE($ROPEB_DIR/include)

  dnl # --with-ropeb -> check for lib and symbol presence
  dnl LIBNAME=ropeb # you may want to change this
  dnl LIBSYMBOL=ropeb # you most likely want to change this

  dnl PHP_CHECK_LIBRARY($LIBNAME,$LIBSYMBOL,
  dnl [
  dnl   PHP_ADD_LIBRARY_WITH_PATH($LIBNAME, $ROPEB_DIR/lib, ROPEB_SHARED_LIBADD)
  dnl   AC_DEFINE(HAVE_ROPEBLIB,1,[ ])
  dnl ],[
  dnl   AC_MSG_ERROR([wrong ropeb lib version or lib not found])
  dnl ],[
  dnl   -L$ROPEB_DIR/lib -lm -ldl
  dnl ])
  dnl

  PHP_ADD_LIBRARY(ei, 1, ROPEB_SHARED_LIBADD)
  PHP_NEW_EXTENSION(ropeb, ropeb.c , $ext_shared)
  PHP_SUBST(ROPEB_SHARED_LIBADD)

fi
