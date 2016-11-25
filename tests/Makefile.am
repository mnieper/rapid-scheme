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
AM_CPPFLAGS = -I$(top_srcdir)/common
LDADD = ../common/librapidcommon.la

TEST_EXTENSIONS = .scm
LOG_COMPILER = $(LIBTOOL) --mode=execute $(VALGRIND)
SCM_LOG_COMPILER = $(SCHEME)

#check_SCRIPTS = dumptest.sh
check_PROGRAMS = simple gauss alloc dump gc gc_dump dump_obj
simple_SOURCES = simple.s
gauss_SOURCES = gauss.s
alloc_SOURCES = alloc.s
dump_SOURCES = dump.s
gc_SOURCES = gc.c gc_util.c
gc_dump_SOURCES = gc_dump.c gc_util.c
simple_LDADD = ../librapid/librapidscheme.la
gauss_LDADD = ../librapid/librapidscheme.la
alloc_LDADD = ../librapid/librapidscheme.la
dump_LDADD = ../librapid/librapidscheme.la

dump_obj_LDADD = ../librapid/librapidscheme.la

TESTS = tests.scm $(check_PROGRAMS)

dump_obj.o: dump
	./dump

.scm.s:
	$(COMPILER) $@ < $<

CLEANFILES = simple.s gauss.s alloc.s
