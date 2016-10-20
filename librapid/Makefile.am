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

GENERATE_GLOBAL_SYMBOLS = ../src/compiler/generate-global-symbols-file

lib_LTLIBRARIES = librapidscheme.la
librapidscheme_la_SOURCES = rapid_main.s
librapidscheme_la_LIBADD = ../lib/libgnu.la

global-symbols.s: ../src/compiler/lib/rapid/compiler/backend/runtime.global-symbols.scm
	$(GENERATE_GLOBAL_SYMBOLS)

rapid_main.s: global-symbols.s

CLEANFILES = global-symbols.s
