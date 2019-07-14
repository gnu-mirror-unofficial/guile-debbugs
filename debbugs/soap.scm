;;; Guile-Debbugs --- Guile bindings for Debbugs
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (debbugs soap)
  #:use-module (debbugs base64)
  #:use-module (debbugs config)
  #:use-module (debbugs cache)
  #:use-module (sxml simple)
  #:use-module (sxml xpath)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (soap-request
            soap-request?
            soap-request-body
            soap-request-callback

            soap-invoke
            soap-invoke*
            soap->scheme))

;; (define (parse-wsdl file)
;;   "Read service wsdl from FILE and return the sxml representation."
;;   (with-input-from-file file
;;     (lambda _ (xml->sxml (current-input-port)
;;                          #:trim-whitespace? #t))))

;; (define (operations wsdl)
;;   ((sxpath '(// http://schemas.xmlsoap.org/wsdl/:operation)) wsdl))

;; (define (messages wsdl)
;;   ((sxpath '(// http://schemas.xmlsoap.org/wsdl/:message)) wsdl))

;; (define-syntax define-message
;;   (lambda (form)
;;     (syntax-case form ()
;;       ((define-message message)
;;        ;;(identifier? #'message)
;;        (begin
;;          (pk (syntax->datum #'message))
;;          (with-syntax
;;              ((name (car ((sxpath '(@ name *text*)) (syntax->datum #'message)))))
;;            (with-syntax
;;                ((proc (datum->syntax #'message (symbol-append 'soap-message: (string->symbol (syntax->datum #'name))))))
;;              #'(define (proc)
;;                  (display name)))))))))

;; (define (make-message-proc message)
;;   (let ((name (string->symbol (car ((sxpath '(@ name *text*)) message)))))
;;     (values (lambda (args)
;;               (display name))
;;             name)))

(define-record-type <soap-request>
  (make-soap-request body callback)
  soap-request?
  (body     soap-request-body)
  (callback soap-request-callback))

(define* (soap-request body #:optional (callback identity))
  "Return a <soap-request> consisting of the BODY in a SOAP envelope
and an optional CALLBACK procedure for handling a response."
  (make-soap-request
   `(soap:Envelope
     (@ (xmlns:soap            . "http://schemas.xmlsoap.org/soap/envelope/")
        (xmlns:xsi             . "http://www.w3.org/1999/XMLSchema-instance")
        (xmlns:xsd             . "http://www.w3.org/1999/XMLSchema")
        (xmlns:soapenc         . "http://schemas.xmlsoap.org/soap/encoding/")
        (soapenc:encodingStyle . "http://schemas.xmlsoap.org/soap/encoding/"))
     (soap:Body ,body))
   callback))

;; XXX: this is necessary to avoid a "bad-header" error when
;; receiving a Content-type header like this:
;; Content-Type: multipart/related; type="text/xml"; start="<main_envelope>"; boundary="=-=-="

;; Guile's original Content-type handler cannot deal with the boundary
;; string containing "=", so we replace it.
;; See Guile bug#32528.
(declare-header! "Content-Type"
  (lambda (str)
    (let ((parts (string-split str #\;)))
      (cons ((@@ (web http) parse-media-type) (car parts))
            (map (lambda (x)
                   (let ((eq (string-index x #\=)))
                     (unless eq
                       ((@@ (web http) bad-header) 'content-type str))
                     (cons
                      (string->symbol
                       (string-trim x char-set:whitespace 0 eq))
                      (string-trim-right x char-set:whitespace (1+ eq)))))
                 (cdr parts)))))
  (lambda (val)
    (match val
      (((? symbol?) ((? symbol?) . (? string?)) ...) #t)
      (_ #f)))
  (lambda (val port)
    (match val
      ((type . args)
       ((@@ (web http) put-symbol) port type)
       (match args
         (() (values))
         (args
          (put-string port ";")
          ((@@ (web http) put-list)
           port args
           (lambda (port pair)
             (match pair
               ((k . v)
                ((@@ (web http) put-symbol) port k)
                (put-char port #\=)
                (put-string port v))))
           ";")))))))

(define (soap-invoke instance op . args)
  "Build a SOAP request from the SOAP operation OP and the arguments
ARGS, and send the request to the SOAP service of the specified
INSTANCE.  Process the response with the request's callback or return
the SXML response body."
  (let* ((uri (if (string? instance) instance
                  (instance 'soap)))
         (request (apply op args))
         (req-xml (call-with-output-string
                    (lambda (port)
                      (sxml->xml (soap-request-body request) port)))))
    (receive (response body-port)
        (http-post uri
                   #:body req-xml
                   #:headers
                   `((content-type . (text/xml))
                     (content-length . ,(string-length req-xml)))
                   #:streaming? #t
                   #:decode-body? #t)
      ((soap-request-callback request)
       (xml->sxml body-port #:trim-whitespace? #t)))))

(define (soap-invoke* . args)
  "Cache the return value of SOAP-INVOKE.  Return the cached value if
it is still fresh."
  (or (cached? args)
      (cache! args (apply soap-invoke args))))

(define* (soap->scheme sxml #:optional (plain #f))
  "Convert a SOAP sxml expression for a named value to a Scheme value.
If PLAIN is #T return only the value, otherwise return a pair of a
name and the value."
  ;; sxml-match cannot match against arbitrary tag names, so we need
  ;; to use sxpath instead.
  (let* ((converter (match ((sxpath '(@ http://www.w3.org/1999/XMLSchema-instance:type *text*)) sxml)
                      (("xsd:string") identity)
                      (("xsd:base64Binary")
                       ;; Debbugs does not tell us what encoding is
                       ;; used for the body, so we first try UTF-8 and
                       ;; fall back to ISO 8859-1 if decoding failed.
                       (compose
                        (lambda (decoded)
                          (catch 'decoding-error
                            (lambda () (bytevector->string decoded "UTF-8"))
                            (lambda _  (bytevector->string decoded "ISO 8859-1"))))
                        base64-decode))
                      (("xsd:int") string->number)
                      (("soapenc:Array") (cut soap->scheme <> #t))
                      (("apachens:Map")
                       (lambda (item)
                         (let ((key   ((select-kids (node-typeof? 'urn:Debbugs/SOAP:key)) item))
                               (value ((select-kids (node-typeof? 'urn:Debbugs/SOAP:value)) item)))
                           (list (cons (soap->scheme key #t)
                                       (soap->scheme value #t))))))
                      (_ identity)))
         (key       (car sxml))
         (value     (match ((select-kids (lambda (node)
                                           (not ((node-typeof? '@) node)))) sxml)
                      ('() #f)
                      ((val) (converter val))
                      ((and (val ...) vals)
                       (map converter vals)))))
    (if plain value
        (cons (string->symbol
               (string-map (match-lambda
                             (#\_ #\-)
                             (c c))
                           (last (string-split (symbol->string key) #\:))))
              value))))
