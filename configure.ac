#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.61)
AC_INIT(natter, 0.1.0)
AC_CONFIG_SRCDIR([src/natter_expat.c])

# Checks for programs.
AC_PROG_CC

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([string.h, expat.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([memmove memset])

# Erlang stuff
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_CHECK_LIB(erl_interface,
                    [], [AC_MSG_FAILURE(cannot find erl_interface, [1])])

# Expat stuff
AC_CHECK_LIB(expat, XML_ParserCreate,
             [], [AC_MSG_FAILURE(cannot find libexpat, [1])])

# Output files
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
