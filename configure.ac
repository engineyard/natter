# -*- Autoconf -*-

# Required versions
AC_PREREQ(2.61)

# Intialization
AC_INIT(natter, 0.2.0)
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
ifdef([LT_INIT], [LT_INIT], [AC_PROG_LIBTOOL])
AC_SUBST([LIBTOOL_DEPS])

# Check our source distribution
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

# There is a bug in the Erlang support in autoconf versions after 2.61. So, in
# order to provide a consistent experience, this is a workaround. The bug has
# been fixed, but a new version has not yet been released.
m4_define([AC_LANG(Erlang)],
[ac_ext=erl
 ac_compile='$ERLC $ERLCFLAGS -b beam conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
 ac_link='$ERLC $ERLCFLAGS -b beam conftest.$ac_ext >&AS_MESSAGE_LOG_FD; echo "#!/bin/sh" > conftest$ac_exeext; echo "\"$ERL\" -s conftest -s init stop -noshell" >> conftest$ac_exeext; chmod +x conftest$ac_exeext'
])

AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_CHECK_LIB(erl_interface,
                    [], [AC_MSG_FAILURE(cannot find erl_interface, [1])])

# Expat stuff
AC_CHECK_LIB(expat, XML_ParserCreate,
             [], [AC_MSG_FAILURE(cannot find libexpat, [1])])

# Output files
AC_CONFIG_FILES([Makefile
                 src/Makefile
                 tests/Makefile])
AC_OUTPUT
