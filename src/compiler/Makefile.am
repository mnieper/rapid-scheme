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

noinst_SCRIPTS = scheme compile-stale.scm rapid-compiler

LIBRARY_FILES = lib/rapid/and-let.scm lib/rapid/and-let.sld		\
lib/rapid/and-let/test.sld lib/rapid/args-fold.scm			\
lib/rapid/args-fold.sld lib/rapid/args-fold/test.sld			\
lib/rapid/base.sld lib/rapid/boxes.scm lib/rapid/boxes.sld		\
lib/rapid/boxes/test.sld lib/rapid/comparators.scm			\
lib/rapid/cps-transform.sld lib/rapid/cps-transform/test.sld		\
lib/rapid/comparators.sld lib/rapid/comparators/test.sld		\
lib/rapid/compiler.scm lib/rapid/compiler.sld lib/rapid/error.scm	\
lib/rapid/error/test.sld lib/rapid/expand-library.scm			\
lib/rapid/expand-library.sld lib/rapid/expand-library/test.sld		\
lib/rapid/expand.scm lib/rapid/expand.sld lib/rapid/expand/test.sld	\
lib/rapid/expressions.scm lib/rapid/expressions.sld			\
lib/rapid/expressions/test.sld lib/rapid/features.scm			\
lib/rapid/features.sld lib/rapid/features/test.sld			\
lib/rapid/fix-letrec.scm lib/rapid/fix-letrec.sld			\
lib/rapid/fix-letrec/test.sld lib/rapid/format.scm			\
lib/rapid/format.sld lib/rapid/format/test.sld				\
lib/rapid/generators.scm lib/rapid/generators.sld			\
lib/rapid/generators/test.sld lib/rapid/graphs.scm			\
lib/rapid/graphs.sld lib/rapid/graphs/test.sld				\
lib/rapid/identifiers.scm lib/rapid/identifiers.sld			\
lib/rapid/identifiers/test.sld lib/rapid/immutable-maps.scm		\
lib/rapid/immutable-maps.sld lib/rapid/immutable-maps/test.sld		\
lib/rapid/immutable-sets.scm lib/rapid/immutable-sets.sld		\
lib/rapid/immutable-sets/test.sld lib/rapid/import-sets.scm		\
lib/rapid/import-sets.sld lib/rapid/import-sets/test.sld		\
lib/rapid/lambda-lift.scm lib/rapid/lambda-lift.sld			\
lib/rapid/lambda-lift/test.sld lib/rapid/library-definitions.scm	\
lib/rapid/library-definitions.sld					\
lib/rapid/library-definitions/test.sld lib/rapid/list-queues.scm	\
lib/rapid/list-queues.sld lib/rapid/list-queues/test.sld		\
lib/rapid/lists.scm lib/rapid/lists.sld lib/rapid/lists/test.sld	\
lib/rapid/macro-transformers.scm lib/rapid/macro-transformers.sld	\
lib/rapid/macro-transformers/test.sld lib/rapid/parameter-objects.scm	\
lib/rapid/parameter-objects.sld lib/rapid/parameter-objects/test.sld	\
lib/rapid/paths.scm lib/rapid/paths.sld lib/rapid/paths/test.sld	\
lib/rapid/primitive-environment.scm					\
lib/rapid/primitive-environment.sld					\
lib/rapid/primitive-environment/test.sld lib/rapid/primitive.scm	\
lib/rapid/primitive.sld lib/rapid/read.scm lib/rapid/read.sld		\
lib/rapid/read/test.sld lib/rapid/receive.scm lib/rapid/receive.sld	\
lib/rapid/receive/test.sld lib/rapid/records.scm			\
lib/rapid/records.sld lib/rapid/records/test.sld			\
lib/rapid/syntactic-environments.scm					\
lib/rapid/syntactic-environments.sld					\
lib/rapid/syntactic-environments/test.sld lib/rapid/syntax.scm		\
lib/rapid/syntax.sld lib/rapid/syntax/test.sld lib/rapid/test.scm	\
lib/rapid/test.sld lib/rapid/test/test.sld lib/rapid/vectors.scm	\
lib/rapid/vectors.sld lib/rapid/vectors/test.sld			\
lib/rapid/version-etc.scm lib/rapid/version-etc.sld

scheme: Makefile compile-stale.scm $(LIBRARY_FILES)
	cd lib; larceny -path . -r7rs -program "../compile-stale.scm"
	echo "#! /bin/sh" > $@
	echo "PROGRAM=\"\$$1\"" >> $@
	echo "shift" >> $@
	echo "larceny -path $(abs_top_srcdir)/src/compiler/lib -r7rs -program \"\$${PROGRAM}\" -- \"\$$@\"" >> $@
	chmod +x $@

compile-stale.scm: Makefile
	echo "(import (larceny compiler))" > $@
	echo "(compile-stale-libraries)" >> $@

rapid-compiler: Makefile
	echo "#! /bin/sh" > $@
	echo -n "$(SCHEME) " >> $@
	echo "$(abs_top_srcdir)/src/compiler/rapid-compiler.scm \"\$$@\"" >> $@
	chmod a+x rapid-compiler

lib/rapid/features.scm: lib/rapid/features.scm.in Makefile
	sed \
	-e 's,[@]PACKAGE[@],$(PACKAGE_NAME),g' \
	-e 's,[@]VERSION[@],$(PACKAGE_VERSION),g' \
	-e 's,[@]BUGREPORT[@],$(PACKAGE_BUGREPORT),g' \
	< $(srcdir)/lib/rapid/features.scm.in > lib/rapid/features.scm

CLEANFILES = $(noinst_SCRIPTS) lib/rapid/features.scm
