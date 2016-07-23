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

LARCENY = larceny

SCHEME = $(LARCENY) -path . -r7rs -program

SCRIPTS = rapid-compiler rapid-scheme compile-stale.scm

all: compile rapid-scheme rapid-compiler

compile: compile-stale.scm
	cd rapid && $(LARCENY) -path .. -r7rs -quiet -program ../compile-stale.scm

compile-stale.scm: Makefile
	echo "(import (larceny compiler))" > $@
	echo "(compile-stale-libraries)" >> $@

rapid-compiler: Makefile
	echo -n "$(SCHEME) " > $@
	echo "rapid-compiler.scm -- \"\$$@\"" >> $@
	chmod a+x rapid-compiler

rapid-scheme: Makefile
	echo -n "$(SCHEME) " > $@
	echo "rapid-scheme.scm -- \"\$$@\"" >> $@
	chmod a+x rapid-scheme

check: compile rapid-scheme unit-tests integration-tests compiler-tests meta-tests

unit-tests:
	$(SCHEME) tests.scm

compiler-tests:
	./rapid-scheme -Ishare data/r7rs-tests.scm

integration-tests:
	./rapid-scheme -Ishare examples/hello-world.scm | grep "Hello, world!"
	./rapid-scheme -Ishare examples/syntax-parameters.scm | grep "10"
	./rapid-scheme data/syntax-error.scm 2>&1 | grep "identifier ‘unknown’ not bound"
	./rapid-scheme data/macro1.scm 2>&1 | grep "duplicate literal identifier"
	./rapid-scheme -I share data/ellipsis.scm 2>&1 | grep "OK OK"
	./rapid-scheme -I share data/meaning.scm 2>&1 | grep "cannot be changed"
	./rapid-scheme -I share data/literals.scm 2>&1 | grep "++++"
	./rapid-scheme -I share data/set.scm 2>&1 | grep "bad identifier"

meta-tests:
	./rapid-scheme -Ishare tests.scm

clean:
	rm -rf $(SCRIPTS)
	find . -type f -name '*.slfasl' -exec rm {} +
