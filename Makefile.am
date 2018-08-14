# Guile-Debbugs --- Guile bindings for Debbugs
# Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
#
# This file is part of Guile-Debbugs.
#
# Guile-Debbugs is free software: you can redistribute it and/or
# modify it under the terms of the GNU  General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# Guile-Debbugs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Guile-Debbugs.  If not, see <http://www.gnu.org/licenses/>.

include guile.am

moddir=$(prefix)/share/guile/site/@GUILE_EFFECTIVE_VERSION@
godir=$(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/ccache

SOURCES =									\
  debbugs/base64.scm						\
  debbugs/bug.scm							\
  debbugs/config.scm						\
  debbugs/email.scm							\
  debbugs/operations.scm 					\
  debbugs/rfc822.scm	 					\
  debbugs/soap.scm

TEST_EXTENSIONS = .scm

SCM_TESTS =									\
  tests/operations.scm						\
  tests/rfc822.scm

TESTS = $(SCM_TESTS)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0

EXTRA_DIST += \
  env.in									\
  tests/responses/README					\
  tests/responses/newest-bugs1.xml			\
  tests/responses/get-status1.xml			\
  tests/requests/README						\
  tests/requests/get-status.xml				\
  tests/requests/newest-bugs.xml			\
  tests/requests/get-bugs.xml				\
  build-aux/test-driver.scm					\
  $(SCM_TESTS)

SCM_LOG_DRIVER =							\
  $(top_builddir)/env						\
  $(GUILE) --no-auto-compile -e main		\
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

info_TEXINFOS = doc/guile-debbugs.texi

EXTRA_DIST +=								\
  doc/fdl-1.3.texi

dist-hook: gen-ChangeLog
gen-ChangeLog:
	$(AM_V_GEN)if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;							\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;		\
	fi

.PHONY: gen-ChangeLog
