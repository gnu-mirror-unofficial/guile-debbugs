;;; Guile-Debbugs --- Guile bindings for Debbugs
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (email email)
  #:use-module (sxml xpath)
  #:use-module (sxml match)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 binary-ports)
  #:use-module (web client)
  #:export
  (newest-bugs
   get-status
   get-bugs
   get-bug-log
   get-usertag
   search-est

   fetch-mbox))

(define (soap-email->email email-item)
  "Convert an SXML expression representing an email item from a SOAP
response to an <email> object."
  (define (drop-lines str k)
    (if (zero? k)
        str
        (drop-lines (substring str (1+ (string-index str #\newline)))
                    (1- k))))

  (let ((email-properties (map soap->scheme (cdr email-item))))
    (let* ((headers (parse-email-headers
                     (drop-lines (assoc-ref email-properties 'header) 2)))
           (content-type (assoc-ref headers 'content-type))
           (body (assoc-ref email-properties 'body)))
      (case (assoc-ref content-type 'type)
        ((multipart)
         (let ((boundary (assoc-ref content-type 'boundary)))
           (make-email
            headers
            ;; Sometimes the debbugs SOAP API provides only the first
            ;; MIME entity of a multipart message. This is a bug, and
            ;; the following works around it until it can be fixed in
            ;; the debbugs SOAP service.
            (if (string-contains body (string-append "--" boundary))
                (parse-email-body headers body)
                (list (make-mime-entity
                       `(content-type (type . text)
                                      (subtype . plain))
                       body))))))
        (else (make-email headers body))))))

(define (newest-bugs amount)
  "Return a list of bug numbers corresponding to the newest AMOUNT
bugs."
  (soap-request
   `(ns1:newest_bugs
     (@ (xmlns:ns1 . "urn:Debbugs/SOAP")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (amount (@ (xsi:type . "xsd:int")) ,amount))
   (lambda (response-body)
     (map string->number ((sxpath '(// urn:Debbugs/SOAP:item *text*)) response-body)))))

(define (get-status bug-ids)
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

(define (get-bugs args)
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

(define (get-bug-log bug-id)
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

(define (get-usertag email)
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

(define %allowed-attributes
  '(@title severity submitter @author subject status @cdate package tags date))

(define %number-operators
  '((=  . NUMEQ)
    (/= . NUMNE)
    (>  . NUMGT)
    (>= . NUMGE)
    (<  . NUMLT)
    (<= . NUMLE)
    (>< . NUMBT)))

;; We use case insensitive searches for all string operators except
;; for string-regex.
(define %string-operators
  '((string=            . ISTREQ)
    (string-not-equal   . ISTRNE)
    (string-contains    . ISTRINC)
    (string-prefix      . ISTRBW)
    (string-suffix      . ISTREW)
    (string-and         . ISTRAND)
    (string-or          . ISTROR)
    (string-or-equal    . ISTROREQ)
    (string-regex       . STRRX)))

(define (operator op)
  "Translate readable search operators to attribute search conditions
supported by the HyperEstraier search engine, which is used by
debbugs.gnu.org."
  (or (assoc-ref %string-operators op)
      (assoc-ref %number-operators op)))

;; This is not implemented in the Debian deployment of Debbugs.
(define* (search-est phrase
                     #:key (skip 0) (max 0) (attributes '()))
  "Return the result of a full text search according to PHRASE.  When
SKIP is provided the given number of hits will be skipped; this is
useful for paged results.  At most MAX results are returned when MAX
is greater than zero.  The number of returned results is always
limited by the absolute maximum returned by the Debbugs server.

ATTRIBUTES is a list of lists containing the attribute, the value, and
an operator."
  (soap-request
   `(ns1:search_est
     (@ (xmlns:ns1 . "urn:Debbugs/SOAP")
        (xmlns:types . "urn:Debbugs/SOAP/TYPES")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (ns1:query
      (@ (xsi:type "soapenc:Array")
         (soapenc:arrayType "types:ArrayOfAnyType[]"))
      ;; The primary query
      (ns1:query
       (@ (xsi:type "soapenc:Array")
          (soapenc:arrayType "xsd:anyType[6]"))
       ((ns1:query (@ (xsi:type "xsd:string")) "phrase")
        (ns1:query (@ (xsi:type "xsd:string")) ,phrase)
        (ns1:query (@ (xsi:type "xsd:string")) "max")
        (ns1:query (@ (xsi:type "xsd:string")) ,(number->string max))
        (ns1:query (@ (xsi:type "xsd:string")) "skip")
        (ns1:query (@ (xsi:type "xsd:string")) ,(number->string skip))))
      ;; Attribute filters.
      ,@(map (match-lambda
               ((attr op . values)
                `((ns1:query
                   (@ (xsi:type "soapenc:Array")
                      (soapenc:arrayType "xsd:anyType[4]"))
                   ((ns1:query (@ (xsi:type "xsd:string"))
                               ,(symbol->string attr))
                    (ns1:query (@ (xsi:type "xsd:string"))
                               ,(match values
                                  (((? string? s)) s)
                                  (((? number? n)) (number->string n))
                                  (((? number? a) (? number? b))
                                   (string-append (number->string a)
                                                  " "
                                                  (number->string b)))))
                    (ns1:query (@ (xsi:type "xsd:string")) "operator")
                    (ns1:query (@ (xsi:type "xsd:string"))
                               ,(operator op)))))))
             attributes)))
   (lambda (response-body)
     (let ((items ((sxpath '(// urn:Debbugs/SOAP:search_estResponse
                                http://schemas.xmlsoap.org/soap/encoding/:Array
                                urn:Debbugs/SOAP:item)) response-body)))
       ;; Flatten to list of alists
       (map (lambda (item)
              (match (soap->scheme item)
                (('item (pair) ...) pair)))
            items)))))

;; This is not a normal operation.  Call it directly, not via
;; soap-invoke.
(define* (fetch-mbox instance bug-number #:optional msg-num mbox-type
                     #:key streaming?)
  "Download the mbox containing messages of bug BUG-NUMBER from the
Debbugs server INSTANCE (a procedure returning a string when given the
symbol 'email).  If MSG-NUM is not provided, all emails relating to
this BUG-NUMBER are fetched."
  (let* ((options
          `((bug . ,(number->string bug-number))
            ,@(cond
               ((member mbox-type '(mboxmaint mboxstat mboxstatus))
                `((mbox-type . "yes")))
               (else '()))
            ,@(if msg-num
                  `((msg . ,msg-num))
                  '())
            (mbox . "yes")))
         (uri (string-append (instance 'email) "?"
                             (string-join (map (match-lambda
                                                 ((key . value)
                                                  (format #f "~a=~a" key value)))
                                               options)
                                          ";"))))
    (if streaming?
        (http-get uri #:streaming? #t)
        (call-with-values
            (lambda () (http-get uri #:streaming? #t))
          (lambda (response port)
            (with-input-from-port port
              (lambda ()
                (let loop ((bv (get-bytevector-some (current-input-port))))
                  (match bv
                    ((? eof-object?) #t)
                    (bv
                     (put-bytevector (current-output-port) bv)
                     (loop (get-bytevector-some (current-input-port))))))))
            response)))))
