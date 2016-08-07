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

RAPID_SCHEME = ../../src/runtime/rapid-scheme
RAPID_SCHEME_FLAGS = -I../../src/runtime/lib -I../../src/compiler/lib

check_SCRIPTS = check.sh

TESTS = r7rs-tests.scm hello-world.tst syntax-parameters.tst		\
syntax-error.tst macro1.tst ellipsis.tst meaning.tst literals.tst	\
set.tst read.tst ../tests.scm

TEST_EXTENSIONS =  .tst .scm
SCM_LOG_COMPILER = $(RAPID_SCHEME)
AM_SCM_LOG_FLAGS = $(RAPID_SCHEME_FLAGS)
TST_LOG_COMPILER = ./check.sh

check.sh: Makefile
	echo "#! /bin/sh" > $@
	echo "$(RAPID_SCHEME) $(RAPID_SCHEME_FLAGS) \"\$$1\" 2>&1 | grep -q \"\`cat \$$(basename \"\$$1\" .tst).ok\`\"" > $@
	chmod a+x $@

CLEANFILES = $(check_SCRIPTS)
