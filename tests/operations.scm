;;; Guile-Debbugs --- Guile bindings for Debbugs
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-operations)
  #:use-module (debbugs operations)
  #:use-module (debbugs soap)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 rdelim)
  #:use-module (sxml simple))

(test-begin "operations")

(define (asset name)
  (with-input-from-file (string-append (getenv "srcdir")
                                       "/tests/" name)
    read-string))

(test-assert "newest-bugs generates soap request"
  (soap-request? (newest-bugs 2)))

(test-equal "newest-bugs generates XML"
  (string-trim-both (asset "requests/newest-bugs.xml"))
  (let ((op (newest-bugs 2)))
    (with-output-to-string
      (lambda _ (sxml->xml (soap-request-body op))))))

(test-equal "newest-bugs parses response"
  ((soap-request-callback (newest-bugs 3))
   (xml->sxml (asset "responses/newest-bugs1.xml")))
  (list 881351 881352 881353))

(test-end "operations")
