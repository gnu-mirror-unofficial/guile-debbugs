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

(define-module (debbugs bug)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module ((sxml xpath) #:hide (filter))
  #:use-module (debbugs soap)
  #:export (bug?

            bug-package bug-severity bug-num bug-subject bug-summary bug-msgid
            bug-originator bug-owner
            bug-done bug-archived bug-unarchived bug-pending
            bug-blocks bug-blockedby bug-mergedwith bug-affects
            bug-date bug-log-modified bug-last-modified
            bug-forwarded bug-fixed-versions bug-found-versions
            bug-source bug-outlook bug-tags bug-found bug-location

            soap-bug->bug))

(define %ignored-bug-fields
  '(keywords fixed-date found-date id found fixed))

(define-record-type <bug>
  (make-bug
   package severity bug-num subject summary msgid
   originator owner
   done archived unarchived pending
   blocks blockedby mergedwith affects
   date log-modified last-modified
   forwarded fixed-versions found-versions source outlook tags location)
  bug?
  (package  bug-package)
  (severity bug-severity)
  (bug-num  bug-num)
  (subject  bug-subject)
  (summary  bug-summary)
  (msgid    bug-msgid) ; reference to mail

  ;; People
  (originator bug-originator) ; may be base64 encoded
  (owner      bug-owner)

  ;; Status
  (done       bug-done)
  (archived   bug-archived)
  (unarchived bug-unarchived)
  (pending    bug-pending)

  ;; Bug references
  (blocks     bug-blocks)
  (blockedby  bug-blockedby)
  (mergedwith bug-mergedwith)
  (affects    bug-affects) ; ?

  ;; Timestamps
  (date          bug-date)
  (log-modified  bug-log-modified)
  (last-modified bug-last-modified)

  ;; Misc
  (forwarded      bug-forwarded)
  (fixed-versions bug-fixed-versions)
  (found-versions bug-found-versions)
  (source         bug-source)
  (outlook        bug-outlook)
  (tags           bug-tags)
  (location       bug-location))

(set-record-type-printer! <bug>
  (lambda (record port)
    (simple-format port "#<bug ~s ~a>"
                   (bug-num record)
                   (number->string (object-address record) 16))))

(define* (bug #:key
              package severity bug-num subject summary msgid
              originator owner
              done archived unarchived pending
              blocks blockedby mergedwith affects
              date log-modified last-modified
              forwarded fixed-versions found-versions source outlook tags location)
  (make-bug
   package severity bug-num subject summary msgid
   originator owner
   done archived unarchived pending
   blocks blockedby mergedwith affects
   date log-modified last-modified
   forwarded fixed-versions found-versions source outlook tags location))

(define (soap-bug->bug bug-item)
  ;; A bug in the SOAP response is an item with a key and a value.  We
  ;; don't care about the key so we just take all of the children of
  ;; the value expression.
  (let ((bug-properties
         (map soap->scheme ((sxpath '(urn:Debbugs/SOAP:value *any*)) bug-item))))
    (apply bug (append-map (match-lambda
                             ;; timestamps
                             ((and ((or 'date 'log-modified 'last-modified) . _)
                                   (key . value))
                              (list (symbol->keyword key)
                                    (time-utc->date (make-time time-utc 0 value))))
                             ;; booleans
                             ((and ((or 'archived 'unarchived) . _)
                                   (key . value))
                              (list (symbol->keyword key)
                                    (if (number? value)
                                        ((negate zero?) value)
                                        #f)))
                             ;; anything else
                             ((key . value)
                              (list (symbol->keyword key) value)))
                           (filter (match-lambda
                                     ((key . value)
                                      (not (member key %ignored-bug-fields))))
                                   bug-properties)))))
