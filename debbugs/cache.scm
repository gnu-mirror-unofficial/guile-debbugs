;;; Guile-Debbugs --- Guile bindings for Debbugs
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (debbugs cache)
  #:use-module (debbugs config)
  #:use-module (ice-9 match)
  #:export (cached? cache! forget! forget-all!))

(define %cache (make-hash-table))

(define (cached? key)
  "Return the value matching KEY from the cache if it has not yet
expired or return #F."
  (let ((t (current-time)))
    (match (hash-ref %cache key)
      ((#:expires time #:value value)
       (if (< t time) value #f))
      (_ #f))))

(define* (cache! key value
                 #:optional (ttl (config 'cache-ttl)))
  "Store VALUE for the given KEY and mark it to expire after TTL
seconds."
  (let ((t (current-time)))
    (hash-set! %cache key `(#:expires ,(+ t ttl) #:value ,value))
    value))

(define (forget! key)
  "Delete KEY from the cache."
  (hash-remove! %cache key))

(define (forget-all!)
  "Reset the cache."
  (set! %cache (make-hash-table)))
