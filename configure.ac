dnl Copyright (C) 2016  Marc Nieper-Wißkirchen

dnl This file is part of Rapid Scheme.

dnl This program is free software: you can redistribute it and/or
dnl modify it under the terms of the GNU General Public License as
dnl published by the Free Software Foundation, either version 3 of the
dnl License, or (at your option) any later version.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.

dnl You should have received a copy of the GNU General Public License
dnl along with this program.  If not, see
dnl <http://www.gnu.org/licenses/>.

AC_PREREQ([2.69])
AC_INIT([Rapid Scheme], [0.3.0], [marc@rapid-scheme.org], [],
  [https://www.rapid-scheme.org])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14.1])
AC_USE_SYSTEM_EXTENSIONS
gl_PROG_AR_RANLIB
AM_PROG_AR
LT_PREREQ([2.4.2])
LT_INIT([dlopen])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/compiler/rapid-compiler.scm])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
  Makefile
  lib/Makefile
  librapid/Makefile
  src/compiler/Makefile
  src/runtime/Makefile
  doc/Makefile
  tests/Makefile
  tests/runtime/Makefile
])

# Checks for programs.
AC_PROG_CC
gl_EARLY
AC_PROG_CC_STDC
AM_PROG_AS

gl_INIT

# FIXME: Write a macro that locates a suitable Scheme implementation.
AC_SUBST([SCHEME], ['$(abs_top_builddir)/src/compiler/scheme'])

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
