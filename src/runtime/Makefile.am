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

noinst_SCRIPTS = rapid-scheme

rapid-scheme: Makefile
	echo "#! /bin/sh" > $@
	echo -n "$(SCHEME) " >> $@
	echo "$(abs_top_srcdir)/src/runtime/rapid-scheme.scm \"\$$@\"" >> $@
	chmod a+x rapid-scheme

CLEANFILES = $(noinst_SCRIPTS)
