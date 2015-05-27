AC_PREREQ(2.60)
AC_INIT([janus], [0.1.0-`date +%Y%m%d%H%M%S`])
AM_INIT_AUTOMAKE

AC_CANONICAL_HOST

case $host_os in
cygwin* | mingw* | pw32*)
  ON_WIN32=yes
  ;;
esac

AC_SUBST(ON_WIN32)

# Checks for programs.
dnl check for sml
SML_PROG_SML
SML_PROG_SMLCM
SML_CHECK_HEAPIMAGESUFFIX
if test x"$ON_WIN32" = x"yes";then
   SML_CMITEM_CHECK(smlnj-lib.cm, [], [AC_MSG_ERROR(smlnj-lib.cm is not found)])
   SML_CMITEM_CHECK(ml-yacc-lib.cm, [],
				    [AC_MSG_ERROR(ml-yacc-lib.cm is not found)])
   SML_CMITEM_CHECK(inet-lib.cm, sml_have_inet_lib_cm=1, sml_have_inet_lib_cm=0)
   SML_CMITEM_CHECK(unix-lib.cm, sml_have_unix_lib_cm=1, sml_have_unix_lib_cm=0)
   AC_SUBST(sml_have_inet_lib_cm)
   AC_SUBST(sml_have_unix_lib_cm)
# else
   # AC_CHECK_PROG(sml, ml-lex, ml-yacc, ml-build, ml-makedepend)
fi

dnl check executable suffix
AC_EXEEXT

dnl check scirpt suffix
AC_MSG_CHECKING(suffix of script)
if test x"$ON_WIN32" = x"yes";then
		script_suffix=.bat
else
		script_suffix=
fi
AC_MSG_RESULT($script_suffix)
AC_SUBST(script_suffix)

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
