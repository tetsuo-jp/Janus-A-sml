# SML_PROG_SML
# ------------
# check for the existence of sml
# If found, shell variable 'sml' is defined as found program name.
AC_DEFUN([SML_PROG_SML],
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_CHECK_PROG(sml, sml, sml)
if test x"${ac_cv_prog_sml}" = x; then
  case $host_os in
    cygwin* | mingw* | pw32*)
      dnl AC_CHECK_PROG can not find sml.bat on cygwin, so set manually.
      sml=sml.bat ;;
    *)
      AC_MSG_ERROR(none of sml/sml.bat was found.) ;;
  esac
fi
AC_SUBST(sml)])

# SML_PROG_SMLCM
# -----------------------------------------------------
# check for the existence of sml which has CM structure
# If found, shell variable 'smlcm' is defined as found program name.
AC_DEFUN([SML_PROG_SMLCM],
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_CHECK_PROG(smlcm, sml-cm, sml-cm)
if test x"${ac_cv_prog_smlcm}" = x; then
  dnl ToDo :check sml has CM structure
  AC_CHECK_PROG(smlcm, sml, sml)
  if test x"${ac_cv_prog_smlcm}" = x; then
    case $host_os in
      cygwin* | mingw* | pw32*)
        dnl AC_CHECK_PROG can not find sml-cm.bat on cygwin, so set manually.
        smlcm=sml-cm.bat ;;
      *)
        AC_MSG_ERROR(none of sml-cm/sml/sml-cm.bat/sml.bat was found.) ;;
    esac
  fi
fi
AC_SUBST(smlcm)])

# SML_CHECK_HEAPIMAGESUFFIX
# --------------------------
# check suffix of heap image file name.
# As result, shell variable 'smlheapimagesuffix' is defined.
AC_DEFUN([SML_CHECK_HEAPIMAGESUFFIX],
[AC_REQUIRE([SML_PROG_SML])dnl
dnl check suffix of sml heap image file 
if test x$sml != x;then
  AC_MSG_CHECKING([suffix of sml heap image file])
  smlheapimagesuffix=` 
    echo 'SMLofNJ.exportFn ("conftest",fn _=>OS.Process.success);' | $sml >/dev/null 2>&1 
    for f in conftest.*-*;do
      echo $f | sed 's/^.*\.\([[^.]]*\)$/\1/' ;rm $f;break;
    done`
  AC_SUBST(smlheapimagesuffix)
  AC_MSG_RESULT($smlheapimagesuffix)
fi])

# SML_CMITEM_CHECK(CMITEM, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
# ------------------------------------------------------------------
# check whether CM of sml can compile CMITEM.
AC_DEFUN([SML_CMITEM_CHECK],
[AC_REQUIRE([SML_PROG_SMLCM])dnl
AC_MSG_CHECKING([for $1])
sml_cv_cmfilename=`echo $1 | sed 's/\./_/g' | sed 's/-/_/g'`
dnl sml_cv_cmfilename=`echo $1 | $as_tr_sh`
echo 'Group is' > conftest.cm
echo "$1" >> conftest.cm
cmres=`echo "CM.make'(\"conftest.cm\");print \"success\";" | $smlcm 2> /dev/null | grep success`
if test x"$cmres" = x; then
    eval sml_cv_have_cmfile_$sml_cv_cmfilename=no
    ifelse([$3], , :, [$3])
    AC_MSG_RESULT([no])
else
    eval sml_cv_have_cmfile_$sml_cv_cmfilename=yes
    ifelse([$2], , :, [$2])
    AC_MSG_RESULT([yes])
fi
rm conftest.cm])
