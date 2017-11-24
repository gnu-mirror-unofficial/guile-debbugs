;;; Guile-Debbugs --- Guile bindings for Debbugs
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2006 Reed Sheridan
;;; Copyright © 2000-2004 Shiro Kawai
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
;;;
;;; Ported from the rfc822 Chicken egg to Guile by Ricardo Wurmus, 2017.
;;; Ported from Gauche to Chicken by Reed Sheridan.
;;;
;;; This is the license of the original implementation for Gauche:
;;;
;;;   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;; Parser and constructor of the message defined in
;; RFC2822 Internet Message Format
;;         http://www.ietf.org/rfc/rfc2822.txt

(define-module (debbugs rfc822)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-14)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 and-let-star)
  #:use-module ((rnrs base) :version (6) #:select (div-and-mod))
  #:use-module (rnrs io ports)
  #:export (;; Parsing a message header
            rfc822-header->list
            rfc822-header-ref

            ;; Basic field parsers
            rfc822-next-token
            rfc822-field->tokens
            rfc822-skip-cfws
            *rfc822-atext-chars*
            *rfc822-standard-tokenizers*
            rfc822-atom
            rfc822-dot-atom
            rfc822-quoted-string

            ;; Specific field parsers
            rfc822-parse-date
            rfc822-date->date))

;; From Oleg Kiselyov's input-parse.scm
(define* (peek-next-char #:optional (port (current-input-port)))
  "Advance PORT by one character and peek at the following character."
  (read-char port) (peek-char port))

;; Guile does not have a definition of read-token.
(define* (read-token predicate #:optional (port (current-input-port)))
  "Read characters from PORT and call the procedure PREDICATE with
each character until PREDICATE returns #F.  Return a string with the
accumulated characters."
  (let ((r (open-output-string)))
    (define (finish) (get-output-string r))
    (let loop ((c (peek-char port)))
      (cond ((eof-object? c) (finish))
            ((predicate c)
             (write-char (read-char port) r)
             (loop (peek-char port)))
            (else (finish))))))


;;=================================================================
;; Parsers
;;

;;-----------------------------------------------------------------
;; Generic header parser, recognizes folded line and field names
;;
(define* (rfc822-header->list iport #:key strict? (reader (cut read-line <>)))
  "Read a header string from the port IPORT and return an alist of
header keys and values.  When STRICT? is #T abort on parse errors.
READER can be provided if the header string should not be read line by
line."
  (define (accum name bodies r)
    (cons (list name (string-concatenate-reverse bodies)) r))

  (let loop ((r '())
             (line (reader iport)))
    (cond
     ((eof-object? line) (reverse! r))
     ((string-null? line) (reverse! r))
     (else
      (receive (n body)
          (let ((idx (string-index line #\:)))
            (values (substring line 0 idx)
                    (substring line (1+ idx) (string-length line))))
        (let ((name (and-let* (((string? n))
                               (name (string-trim-both n))
                               ((string-every (char-set-union
                                               (ucs-range->char-set 33 57)
                                               (ucs-range->char-set 59 126))
                                              name)))
                      (string-downcase name))))
          (if name
              (let loop2 ((nline (reader iport))
                          (bodies (list (string-trim body))))
                (cond ((eof-object? nline)
                       ;; maybe premature end of the message
                       (if strict?
                           (error "premature end of message header")
                           (reverse! (accum name bodies r))))
                      ((string-null? nline) ; end of the header
                       (reverse! (accum name bodies r)))
                      ((memv (string-ref nline 0) '(#\space #\tab))
                       ;; not so careful for byte strings
                       (loop2 (reader iport) (cons nline bodies)))
                      (else
                       (loop (accum name bodies r) nline))))
              (if strict?
                  (error (format #f "bad header line: ~a" line))
                  (loop r (reader iport))))))))))


(define* (rfc822-header-ref header field-name #:optional default)
  "Return the value assigned to the key FIELD-NAME in the alist
HEADER, or return DEFAULT if the key cannot be found in the header."
  (cond ((assoc field-name header) => cadr)
        (else default)))

;;------------------------------------------------------------------
;; Comments, quoted pairs, atoms and quoted string.  Section 3.2
;;

;; skip comments and white spaces, then returns the head char.

(define (rfc822-skip-cfws input)
  (define (scan c)
    (cond ((eof-object? c) c)
          ((char=? c #\( ) (in-comment (peek-next-char input)))
          ((char-whitespace? c) (scan (peek-next-char input)))
          (else c)))
  (define (in-comment c)
    (cond ((eof-object? c) c)
          ((char=? c #\) ) (scan (peek-next-char input)))
          ((char=? c #\\ ) (read-char input) (in-comment (peek-next-char input)))
          ((char=? c #\( ) (in-comment (in-comment (peek-next-char input))))
          (else (in-comment (peek-next-char input)))))
  (scan (peek-char input)))

;; Basic tokenizers.  Supposed to be used for higher-level parsers.
(define *rfc822-atext-chars*
  ;; A-Za-z0-9!#$%&'*+/=?^_`{|}~-
  ;; Can't use char-set:letter due to Latin-1 extras
  (char-set-union (ucs-range->char-set #x41 #x5B)
                  (ucs-range->char-set #x61 #x7B)
                  char-set:digit
                  (char-set #\! #\# #\$ #\% #\& #\' #\* #\+ #\/
                            #\= #\? #\^ #\_ #\` #\{ #\| #\} #\~ #\-)))

(define (rfc822-atom input)
  (read-token (cut char-set-contains? *rfc822-atext-chars* <>) input))

;; NB: this is loose, but usually OK.
(define (rfc822-dot-atom input)
  (read-token (cut char-set-contains?
                   (char-set-adjoin *rfc822-atext-chars* #\.)
                   <>)
              input))

(define (rfc822-quoted-string input)
  "Read a quoted string from the INPUT port.  This procedure assumes
that the first character in the port is a double quote."
  (let ((r (open-output-string)))
    (define (finish) (get-output-string r))
    (let loop ((c (peek-next-char input)))
      (cond ((eof-object? c) (finish)) ; tolerate missing closing DQUOTE
            ((char=? c #\") (read-char input) (finish)) ; discard DQUOTE
            ((char=? c #\\)
             (let ((c (peek-next-char input)))
               (cond ((eof-object? c) (finish)) ; tolerate stray backslash
                     (else (write-char c r) (loop (peek-next-char input))))))
            (else (write-char c r) (loop (peek-next-char input)))))))

;; Default tokenizer table
(define *rfc822-standard-tokenizers*
  `((,(char-set #\") . ,rfc822-quoted-string)
    (,*rfc822-atext-chars* . ,rfc822-dot-atom)))

;; Returns the next token or EOF
(define* (rfc822-next-token input #:optional (opts *rfc822-standard-tokenizers*))
  "Return the next token that can be read from the INPUT port using
the tokenizers in the OPTS alist."
  (let ((toktab (map (lambda (e)
                       (cond
                        ((char-set? e)
                         (cons e (lambda (x)
                                   (read-token (cut char-set-contains? e <>) x))))
                        (else e)))
                     opts))
        (c (rfc822-skip-cfws input)))
    (cond ((eof-object? c) c)
          ((find (lambda (e) (char-set-contains? (car e) c)) toktab)
           => (lambda (e) ((cdr e) input)))
          (else (read-char input)))))

;; returns a list of tokens, for convenience
(define (rfc822-field->tokens field . opts)
  "Return a list of RFC822 tokens read from the header string FIELD
using the optional tokenizer table OPTS."
  (call-with-input-string field
    (lambda (ip)
      (let ((fn (cut apply rfc822-next-token <> opts)))
        (let loop ((acc '()))
          (let ((token (fn ip)))
            (if (eof-object? token)
                (reverse! acc)
                (loop (cons token acc)))))))))

;;------------------------------------------------------------------
;; Date and time, section 3.3
;;

;; Takes RFC-822 type date string, and returns eight values:
;;   year, month, day-of-month, hour, minutes, seconds, timezone, day-of-week.
;; Timezone is an offset from UT in minutes.  Day-of-week is a day from
;; sunday, and may be #f if that information is not available.
;; If the string is not parsable, all the elements are #f.

;; NB: This function follows the new definition of date format in RFC2822,
;; but may fail to recognize "obsolete" format, which allows arbitrary
;; comments appear between words.

;; RPS this port fails the "old tz" test in Gauche's rfc.scm
;; Apparently pcre and Gauche regexes are slightly incompatible
;; But the failed test is apparently not compliant to RFC822 anyway.
;; I really don't know what "old tz" is and where it's used anyway.

(define (rfc822-parse-date string)
  (define (dow->number dow)
    (list-index (cut string=? <> dow)
                '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))
  (define (mon->number mon)
    (+ 1 (list-index (cut string=? <> mon)
                     '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))))
  (define (year->number year)     ; see obs-year definition of RFC2822
    (let ((y (string->number year)))
      (and y
           (cond ((< y 50)  (+ y 2000))
                 ((< y 100) (+ y 1900))
                 (else y)))))
  (define (tz->number tz)
    (cond ((equal? tz "-0000") #f) ; no effective TZ info; see 3.3 of RFC2822
          ((string->number tz))
          ((assoc tz '(("UT" . 0) ("GMT" . 0) ("EDT" . -400) ("EST" . -500)
                       ("CDT" . -500) ("CST" . -600) ("MDT" . -600)
                       ("MST" . -700) ("PDT" . -700) ("PST" . -800)))
           => cdr)
          (else #f)))

  (let* ((pattern "((Sun|Mon|Tue|Wed|Thu|Fri|Sat)[[:space:]]*,)?[[:space:]]*([[:digit:]]+)[[:space:]]*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[[:space:]]*([[:digit:]][[:digit:]]([[:digit:]][[:digit:]])?)[[:space:]]+([[:digit:]][[:digit:]])[[:space:]]*:[[:space:]]*([[:digit:]][[:digit:]])([[:space:]]*:[[:space:]]*([[:digit:]][[:digit:]]))?([[:space:]]+([+-][[:digit:]][[:digit:]][[:digit:]][[:digit:]]|[A-Z][A-Z][A-Z]?))?")
         (m (string-match pattern string)))
    (if m
        (match (map (lambda (n)
                      (match:substring m n))
                    (iota (match:count m) 0))
          ((_ _ dow dom mon yr _ hour min _ sec _ tz)
           (values (year->number yr)
                   (mon->number mon)
                   (string->number dom)
                   (string->number hour)
                   (string->number min)
                   (and sec (string->number sec))
                   (and tz (tz->number tz))
                   (and dow (dow->number dow))))
          (_ (values #f #f #f #f #f #f #f #f)))
        (values #f #f #f #f #f #f #f #f))))

(define (rfc822-date->date string)
  (receive (year month day hour min sec tz . rest)
      (rfc822-parse-date string)
    (and year
         (make-date 0 sec min hour day month year
                    (receive (quot rem) (div-and-mod tz 100)
                      (+ (* quot 3600) (* rem 60)))))))

;;------------------------------------------------------------------
;; Address specification (Section 3.4)
;;

;; The EBNF syntax in RFC2822 requires arbitrary lookahead,
;; so straight recursive-descent parser won't work.
;;

;; TODO: to be written

