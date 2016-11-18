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

COMPILER = $(SCHEME) compiler.scm
LDADD = ../librapid/librapidscheme.la

TEST_EXTENSIONS = .scm
LOG_COMPILER = $(LIBTOOL) --mode=execute $(VALGRIND)
SCM_LOG_COMPILER = $(SCHEME)

check_PROGRAMS = simple gauss alloc
simple_SOURCES = simple.s
gauss_SOURCES = gauss.s
alloc_SOURCES = alloc.s

TESTS = tests.scm $(check_PROGRAMS)

.scm.s:
	$(COMPILER) $@ < $<

CLEANFILES = simple.s
