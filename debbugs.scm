;;; Guile-Debbugs --- Guile bindings for Debbugs
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This file is part of Guile-Debbugs.
;;;
;;; Guile-Debbugs is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-Debbugs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-Debbugs.  If not, see <http://www.gnu.org/licenses/>.

(define-module (debbugs)
  #:use-module (debbugs soap)
  #:use-module (debbugs bug)
  #:use-module (debbugs operations)
  #:use-module (ice-9 match)
  #:export
  (%debian %gnu))

(define* (%debian #:optional (type 'soap))
  (match type
    ('soap  "https://bugs.debian.org/cgi/soap.cgi")
    ('email "https://bugs.debian.org/cgi-bin/bugreport.cgi")))

(define* (%gnu #:optional (type 'soap))
  (match type
    ('soap  "https://debbugs.gnu.org/cgi/soap.cgi")
    ('email "https://debbugs.gnu.org/cgi-bin/bugreport.cgi")))

;; Export public bindings from other modules for convenience.
(for-each (let ((i (module-public-interface (current-module))))
            (lambda (m)
              (module-use! i (resolve-interface
                              `(debbugs ,m)))))
          '(bug operations soap))
