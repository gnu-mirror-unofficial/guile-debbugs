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

(define-module (debbugs email)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (debbugs soap)
  #:use-module (debbugs rfc822)
  #:export (email?
            email-headers
            email-body
            email-msg-num
            email-attachments

            soap-email->email))

(define-record-type <email>
  (make-email headers body msg-num attachments)
  email?
  (headers      email-headers)
  (body         email-body)
  (msg-num      email-msg-num)
  (attachments  email-attachments))

(set-record-type-printer! <email>
  (lambda (record port)
    (simple-format port "#<email ~s ~a>"
                   (email-msg-num record)
                   (number->string (object-address record) 16))))

(define (parse-headers header-text)
  "Parse the email headers and return them as an alist."
  (with-input-from-string header-text
    (lambda () (rfc822-header->list (current-input-port)))))

(define* (email #:key header body msg-num (attachments '()))
  (make-email (parse-headers header) body msg-num attachments))

(define (soap-email->email email-item)
  "Convert an SXML expression representing an email item from a SOAP
response to an <email> object."
  (let ((email-properties (map soap->scheme (cdr email-item))))
    (apply email
           (append-map (match-lambda
                         ((key . value)
                          (list (symbol->keyword key) value)))
                       email-properties))))
