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
  #:use-module (debbugs email)
  #:use-module (sxml xpath)
  #:use-module (sxml match)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs))

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

(define-public (get-bugs args)
  "Returns bug numbers for bugs that match the conditions given by
ARGS, an alist of key-value pairs.  Possible keys: package, submitter,
maint, src, severity, status (which can be 'done', 'forwarded',
'open'), tag, owner, bugs, affects, archive (which can be 'both', or a
Boolean value)."
  (soap-request
   `(ns1:get_bugs
     (@ (xmlns:ns1 . "urn:Debbugs/SOAP")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (ns1:query
      (@ (xsi:type "soapenc:Array")
         (soapenc:arrayType ,(string-append "xsd:anyType[" (number->string (* 2 (length args))) "]")))
      ,@(map (match-lambda
               ((key . value)
                `((ns1:query (@ (xsi:type "xsd:string")) ,key)
                  (ns1:query (@ (xsi:type "xsd:string")) ,value))))
             args)))
   (lambda (response-body)
     (map string->number ((sxpath '(// urn:Debbugs/SOAP:item *text*)) response-body)))))

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
       (map soap-email->email emails)))))

(define-public (get-usertag email)
  "Return an association list of tag names to lists of bug numbers for
all bugs that have been tagged by EMAIL."
  (soap-request
   `(ns1:get_usertag
     (@ (xmlns:ns1 . "urn:Debbugs/SOAP")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (ns1:user
      (@ (xsi:type "xsd:string")) ,email))
   (lambda (response-body)
     (let ((response ((sxpath '(// urn:Debbugs/SOAP:get_usertagResponse *)) response-body)))
       ;; The problem here is that for some usertags (e.g. usertags
       ;; for "hertzog@debian.org") a map is returned (so I could use
       ;; soap->scheme), but in other cases (e.g. usertags for
       ;; "aj@azure.humbug.org.au") each tag is an array with bug-num
       ;; items.
       (sxml-match (car response)
        ((urn:Debbugs/SOAP:s-gensym3 (@ (http://www.w3.org/1999/XMLSchema-instance:type "apachens:Map")) ,items ...)
         (map car (soap->scheme (car response) #t)))
        ((urn:Debbugs/SOAP:s-gensym3 ,tags ...)
         (map (lambda (tag)
                ;; For consistency make sure that the keys are strings.
                (let ((pair (soap->scheme tag)))
                  (if (symbol? (car pair))
                      (cons (symbol->string (car pair))
                            (cdr pair))
                      pair)))
              tags)))))))

;; This is not implemented in the Debian deployment of Debbugs.
(define*-public (search-est phrase #:optional (skip 0) (max #f))
  "Return the result of a full text search according to PHRASE.  When
SKIP is provided the given number of hits will be skipped; this is
useful for paged results.  At most MAX results are returned when MAX
is provided.  The number of returned results is always limited by the
absolute maximum returned by the Debbugs server."
  (soap-request
   `(ns1:search_est
     (@ (xmlns:ns1 . "urn:Debbugs/SOAP")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (ns1:phrase
      (@ (xsi:type "xsd:string")) ,phrase)
     (ns1:skip
      (@ (xsi:type "xsd:int")) ,skip)
     ,@(if max `(ns1:max
                 (@ (xsi:type "xsd:int")) ,max)
           '()))
   ;; TODO: implement response hander
   ))
