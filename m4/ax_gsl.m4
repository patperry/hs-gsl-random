# AX_GSL([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
# -----------------------------------------------

AC_DEFUN([AX_GSL], [
AC_REQUIRE([AX_CBLAS])
ax_gsl_ok=no

AC_ARG_WITH(gsl,
        [AC_HELP_STRING([--with-gsl=<lib>], [use GSL library <lib>])])
case $with_gsl in
        yes | "") ;;
        no) ax_gsl_ok=disable ;;
        -* | */* | *.a | *.so | *.so.* | *.o) GSL_LIBS="$with_gsl" ;;
        *) GSL_LIBS="-l$with_gsl" ;;
esac

AC_CHECK_LIB([m],[cos])

AX_CBLAS([],
    [AC_CHECK_LIB([gslcblas],[cblas_dgemm],
        [CBLAS_LIBS="-lgslcblas"])])

AC_CHECK_LIB([gsl],[gsl_blas_dgemm],
    [ax_gsl_ok=yes; GSL_LIBS="-lgsl"],
    [],$CBLAS_LIBS)

AC_SUBST(GSL_LIBS)

if test x"$ax_gsl_ok" = xyes; then
ifelse([$1],,AC_DEFINE(HAVE_GSL,1,[Define if you have GSL library.]),[$1])
        :
else
        ax_gsl_ok=no
        $2
fi
])dnl AX_GSL
