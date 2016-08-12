# Copyright (C) 2016  Marc Nieper-Wißkirchen

# This file is part of Rapid Scheme.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

check_SCRIPTS = compiler-test.sh runtime-test.sh check.sh

RAPID_COMPILER = ../src/compiler/rapid-compiler
RAPID_FLAGS = -I../src/runtime/lib -I../src/compiler/lib

TESTS = runtime/r7rs-tests.scm tests.scm compiler-test.sh	\
runtime-test.sh hello-world.tst procedure.tst apply.tst		\
call-with-current-continuation.tst continuation-marks.tst	\
multiple-values.tst assignment.tst procedure-assignment.tst	\
define-record-type.tst
TEST_EXTENSIONS = .scm .tst
SCM_LOG_COMPILER = $(SCHEME)
TST_LOG_COMPILER = ./check.sh

compiler-test.sh: Makefile
	echo "#! /bin/sh" > $@
	echo "../src/compiler/rapid-compiler --help | grep -q \"display this help and exit\"" >> $@
	chmod a+x $@

runtime-test.sh: Makefile
	echo "#! /bin/sh" > $@
	echo "../src/runtime/rapid-scheme --help | grep -q \"display this help and exit\"" >> $@
	chmod a+x $@

check.sh: Makefile
	echo "#! /bin/sh" > $@
	echo "test=\$$(basename \"\$$1\" .tst)" >> $@
	echo "$(RAPID_COMPILER) $(RAPID_FLAGS) \"\$$1\" > \$$test.bin" >> $@
	echo "if test \$$? != 0; then" >> $@
	echo "  exit 99" >> $@
	echo "fi" >> $@
	echo "$(SCHEME) \$$test.bin | tr -d \\\\r > \$$test.out" >> $@
	echo "if test \$$? != 0; then" >> $@
	echo "  rm \$$test.bin" >> $@
	echo "  exit 99" >> $@
	echo "fi" >> $@
	echo "cmp -s \$$srcdir/\$$test.ok \$$test.out" >> $@
	echo "if test \$$? != 0; then" >> $@
	echo "  diff \$$srcdir/\$$test.ok \$$test.out" >> $@
	echo "  rm \$$test.bin \$$test.out" >> $@
	echo "  exit 1" >> $@
	echo "fi" >> $@
	echo "rm \$$test.bin \$$test.out" >> $@
	chmod a+x $@

CLEANFILES = $(check_SCRIPTS)
