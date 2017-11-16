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
;;;
;;; This is an adaptation of "ext/rfc/test.scm" from Gauche Scheme.
;;; The file is under the BSD-3 license, reproduced below:
;;;
;;;   Copyright (c) 2000-2017  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;    1. Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;    2. Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;
;;;    3. Neither the name of the authors nor the names of its contributors
;;;       may be used to endorse or promote products derived from this
;;;       software without specific prior written permission.
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

(define-module (test-rfc822)
  #:use-module (debbugs rfc822)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive))

(test-begin "rfc822")


(define rfc822-header1
  "Received: by foo.bar.com id ZZZ55555; Thu, 31 May 2001 16:38:04 -1000 (HST)
Received: from ooo.ooo.com (ooo.ooo.com [1.2.3.4])
\tby foo.bar.com (9.9.9+3.2W/3.7W-) with ESMTP id ZZZ55555
\tfor <yoo@bar.com>; Thu, 31 May 2001 16:38:02 -1000 (HST)
Received: from zzz ([1.2.3.5]) by ooo.ooo.com  with Maccrosoft SMTPSVC(5.5.1877.197.19);
\t Thu, 31 May 2001 22:33:16 -0400
Message-ID: <beefbeefbeefbeef@ooo.ooo.com>
Subject: Bogus Tester
From: Bogus Sender <bogus@ooo.com>
To: You <you@bar.com>, Another <another@ooo.com>
Date: Fri, 01 Jun 2001 02:37:31 (GMT)
Mime-Version: 1.0
Content-Type: text/html
Content-Transfer-Encoding: quoted-printable
X-MSMail-Priority: Normal
X-mailer: FooMail 4.0 4.03 (SMT460B92F)
Content-Length: 4349

")

(define rfc822-header1-list
  '(("received" "by foo.bar.com id ZZZ55555; Thu, 31 May 2001 16:38:04 -1000 (HST)")
    ("received" "from ooo.ooo.com (ooo.ooo.com [1.2.3.4])\tby foo.bar.com (9.9.9+3.2W/3.7W-) with ESMTP id ZZZ55555\tfor <yoo@bar.com>; Thu, 31 May 2001 16:38:02 -1000 (HST)")
    ("received" "from zzz ([1.2.3.5]) by ooo.ooo.com  with Maccrosoft SMTPSVC(5.5.1877.197.19);\t Thu, 31 May 2001 22:33:16 -0400")
    ("message-id" "<beefbeefbeefbeef@ooo.ooo.com>")
    ("subject" "Bogus Tester")
    ("from" "Bogus Sender <bogus@ooo.com>")
    ("to" "You <you@bar.com>, Another <another@ooo.com>")
    ("date" "Fri, 01 Jun 2001 02:37:31 (GMT)")
    ("mime-version" "1.0")
    ("content-type" "text/html")
    ("content-transfer-encoding" "quoted-printable")
    ("x-msmail-priority" "Normal")
    ("x-mailer" "FooMail 4.0 4.03 (SMT460B92F)")
    ("content-length" "4349")
    ))

(test-equal "rfc822-header->list"
  rfc822-header1-list
  (rfc822-header->list (open-input-string rfc822-header1)))

;; token parsers
(test-equal "rfc822-field->tokens (basic)"
  '(("aa") ("bb") ("cc") ("dd") ("ee") (" a\"aa\\aa (a)"))
  (map rfc822-field->tokens
       '("aa"
         "  bb   "
         " (comment) cc(comment)"
         " (co\\mm$$*##&$%ent) dd(com (me) nt)"
         "\"ee\""
         "  \" a\\\"aa\\\\aa (a)\" (comment\\))")))

(test-equal "rfc822-field->tokens"
  '("from" "aaaaa.aaa.org" "by" "ggg.gggg.net" "with" "ESMTP" "id" "24D50175C8")
  (rfc822-field->tokens
   "from aaaaa.aaa.org (aaaaa.aaa.org [192.168.0.9]) by ggg.gggg.net (Postfix) with ESMTP id 24D50175C8"))

(test-equal "rfc822-parse-date"
  '(2003 3 4 12 34 56 -3600 2)
  (receive r (rfc822-parse-date "Tue,  4 Mar 2003 12:34:56 -3600") r))

(test-equal "rfc822-parse-date"
  '(2003 3 4 12 34 56 0 2)
  (receive r (rfc822-parse-date "Tue,  4 Mar 2003 12:34:56 UT") r))

(test-equal "rfc822-parse-date (no weekday)"
  '(2003 3 4 12 34 56 -3600 #f)
  (receive r (rfc822-parse-date "4 Mar 2003 12:34:56 -3600") r))

(test-equal "rfc822-parse-date (no timezone)"
  '(2003 3 4 12 34 56 #f #f)
  (receive r (rfc822-parse-date "4 Mar 2003 12:34:56") r))

(test-equal "rfc822-parse-date (old tz)"
  '(2003 3 4 12 34 56 #f #f)
  (receive r (rfc822-parse-date "4 Mar 2003 12:34:56 jst") r))

(test-equal "rfc822-parse-date (no seconds)"
  '(2003 3 4 12 34 #f 900 #f)
  (receive r (rfc822-parse-date "4 Mar 2003 12:34 +0900") r))

(test-equal "rfc822-parse-date (no seconds)"
  '(2003 3 4 12 34 #f 900 2)
  (receive r (rfc822-parse-date "Tue, 04 Mar 2003 12:34 +0900") r))

(test-equal "rfc822-parse-date (2digit year)"
  '(2003 3 4 12 34 56 -3600 2)
  (receive r (rfc822-parse-date "Tue,  4 Mar 03 12:34:56 -3600") r))

(test-equal "rfc822-parse-date (2digit year)"
  '(1987 3 4 12 34 56 -3600 2)
  (receive r (rfc822-parse-date "Tue,  4 Mar 87 12:34:56 -3600") r))

(test-equal "rfc822-parse-date (Weekday, exhausive)"
  '(0 1 2 3 4 5 6 #f)
  (map-in-order
   (lambda (ind wday)
     (receive (y m d H M S tz wd)
         (rfc822-parse-date
          (format #f "~a, ~a Jan 2000 00:00:00 +0000" wday (+ 2 ind)))
       wd))
   '(0 1 2 3 4 5 6 7)
   '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Znn")))

(test-equal "rfc822-parse-date (Months, exhausive)"
  '(1 2 3 4 5 6 7 8 9 10 11 12 #f)
  (map (lambda (mon)
         (receive (y m d H M S tz wd)
             (rfc822-parse-date
              (format #f "1 ~a 1999 00:00:00 +0000" mon))
           m))
       '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
         "Sep" "Oct" "Nov" "Dec" "Zzz")))

(test-equal "rfc822-parse-date (invalid)"
  '(#f #f #f #f #f #f #f #f)
  (receive r (rfc822-parse-date "Sun 2 Mar 2002") r))

(test-end)
