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

noinst_SCRIPTS = scheme rapid-compiler generate-global-symbols-file

scheme: Makefile
	echo "#! /bin/sh" > $@
	echo "PROGRAM=\"\$$1\"" >> $@
	echo "shift" >> $@
	echo "chibi-scheme -h 100M -I $(abs_top_srcdir)/src/compiler/lib \"\$${PROGRAM}\" \"\$$@\"" >> $@
	chmod +x $@

rapid-compiler: Makefile
	echo "#! /bin/sh" > $@
	echo -n "$(SCHEME) " >> $@
	echo "$(abs_top_srcdir)/src/compiler/rapid-compiler.scm \"\$$@\"" >> $@
	chmod a+x $@

generate-global-symbols-file: Makefile
	echo "#! /bin/sh" > $@
	echo -n "$(SCHEME) " >> $@
	echo "$(abs_top_srcdir)/src/compiler/generate-global-symbols-file.scm \"\$$@\"" >> $@
	chmod a+x $@

CLEANFILES = $(noinst_SCRIPTS)
