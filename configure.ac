
AC_PREREQ(2.61)
AC_INIT([Haskell gsl-random], [0.2], [patperry@stanford.edu])
AC_CONFIG_SRCDIR([gsl-random.cabal])

AC_PROG_CC
AC_CONFIG_MACRO_DIR([m4])

AC_ARG_WITH(compiler, 
    [AC_HELP_STRING([--with-compiler=<cmd>], [use Haskell compiler <cmd>])])

AX_GSL

if test "$ax_gsl_ok" = yes; then
  BUILD_PACKAGE_BOOL=True
else 
  BUILD_PACKAGE_BOOL=False
fi
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_CONFIG_FILES([gsl-random.buildinfo])
AC_OUTPUT
