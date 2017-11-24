dnl Guile-Debbugs --- Guile bindings for Debbugs
dnl Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
dnl
dnl This file is part of Guile-Debbugs.
dnl
dnl Guile-Debbugs is free software: you can redistribute it and/or
dnl modify it under the terms of the GNU  General Public License
dnl as published by the Free Software Foundation, either version 3 of
dnl the License, or (at your option) any later version.
dnl
dnl Guile-Debbugs is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Affero General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with Guile-Debbugs.  If not, see <http://www.gnu.org/licenses/>.

dnl -*- Autoconf -*-

AC_INIT([guile-debbugs], [m4_translit(m4_esyscmd([cat VERSION]),m4_newline)])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([2.2])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.2 installation.])
fi

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_OUTPUT
