# Rapid Scheme --- An implementation of R7RS

# Copyright (C) 2016 Marc Nieper-Wißkirchen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

.POSIX:

.SILENT: check

SCHEME = larceny -path . -r7rs $(SCHEME_FLAGS) -program

SCRIPTS = compile-stale.scm

all: rapid-compiler

compile: compile-stale.scm
	cd rapid && larceny -path .. -r7rs -program ../compile-stale.scm

compile-stale.scm: Makefile
	echo "(import (larceny compiler))" > $@
	echo "(compile-stale-libraries)" >> $@

rapid-compiler: rapid-compiler.scm
	echo "#! /usr/bin/env scheme-script" > $@
	cat $< >> $@
	chmod a+x $@

check: tests
	$(SCHEME) tests.scm

tests: rapid-compiler $(TESTS)
	@! $(SCHEME) rapid-compiler.scm -- -d -Idata data/macros.scm 2>&1 | grep -e "error:" -e note -e info

clean:
	rm -rf rapid-compiler $(SCRIPTS)
