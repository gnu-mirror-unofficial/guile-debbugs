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

(define-module (debbugs operations)
  #:use-module (debbugs soap)
  #:use-module (debbugs bug)
  #:use-module (sxml xpath)
  #:use-module (srfi srfi-1))

(define-public (newest-bugs amount)
  "Return a list of bug numbers corresponding to the newest AMOUNT
bugs."
  (soap-request
   `(ns1:newest_bugs
     (@ (xmlns:ns1 . "urn:Debbugs/SOAP")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (amount (@ (xsi:type . "xsd:int")) ,amount))
   (lambda (response-body)
     (map string->number ((sxpath '(// urn:Debbugs/SOAP:item *text*)) response-body)))))

(define-public (get-status bug-ids)
  "Return <bug> records containing the details for the bugs identified
by BUG-IDS, a list of bug numbers."
  (soap-request
   `(ns1:get_status
     (@ (xmlns:ns1 . "urn:Debbugs/SOAP")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (ns1:bugs
      (@ (xsi:type "soapenc:Array")
         (soapenc:arrayType ,(string-append "xsd:int[" (number->string (length bug-ids)) "]")))
      ,(map (lambda (bug-id)
              `(ns1:bugs (@ (xsi:type "xsd:int")) ,bug-id))
            bug-ids)))
   (lambda (response-body)
     (let ((bugs ((sxpath '(// urn:Debbugs/SOAP:get_statusResponse
                               urn:Debbugs/SOAP:s-gensym3
                               urn:Debbugs/SOAP:item)) response-body)))
       (map soap-bug->bug bugs)))))

(define-public (get-bug-log bug-id)
  "Return emails associated with the bug identified by BUG-ID."
  (soap-request
   `(ns1:get_bug_log
     (@ (xmlns:ns1 . "urn:Debbugs/SOAP")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (ns1:bugnumber
      (@ (xsi:type "xsd:int")) ,bug-id))
   (lambda (response-body)
     (let ((emails ((sxpath '(// urn:Debbugs/SOAP:get_bug_logResponse
                                 http://schemas.xmlsoap.org/soap/encoding/:Array
                                 urn:Debbugs/SOAP:item)) response-body)))
       ;; TODO: parse into record
       emails))))
