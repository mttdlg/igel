;;
;; Extensible any->string
;; depends on: join.scm
;;
;; Copyright 2020 Matteo De Luigi
;;
;;  This file is part of IGEL
;;
;;  IGEL is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  IGEL is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with IGEL.  If not, see <http://www.gnu.org/licenses/>.
;;

(define *any->string:list-of-handlers* '())

(define (any->string:add-handler-first h)
  (set! *any->string:list-of-handlers* (cons h *any->string:list-of-handlers*)))

(define (any->string:add-handler-last h)
  (set! *any->string:list-of-handlers* (append *any->string:list-of-handlers* (list h))))

(define (any->string:extension-handler x)
  (let handler-loop ((handlers-left *any->string:list-of-handlers*))
    (if (null? handlers-left)
      (error-and-exit "any->string received an unsupported argument!") 
      (let* ((result ((car handlers-left) x)))
        (cond
          ((string? result) result)
          ((not result) (handler-loop (cdr handlers-left)))
          (else
            (error "any->string extension handler returned an unsupported type!")))))))

(define (any->string x)
  (define (boolean->string b)
    (if b "#t" "#f"))

   (define (char->string c)
     ; (error "Characters not handled, yet")
     (string-append (list->string (list #\# #\\ c)))) ;; -> work out how to handle slashes

   (define (stringify-list l)
     ;; TODO: what about lists that loop on themselves?
     (apply string-append `("(" ,(join (map any->string l)) ")")))

   (define (stringify-symbol s)
     (string-append "'" (symbol->string s)))

   (cond
      ;; No need to have a hook before checking: one can just
      ;; call any->string from inside a different function, which
      ;; will perform all the pre-checks and call any->string only
      ;; if it has not already pre-handled special cases itself.
      ((   string? x) x)
      ((   number? x) (  number->string x))
      ((   symbol? x) (stringify-symbol x))
      ((  boolean? x) ( boolean->string x))
      ((     char? x) (    char->string x))
      ((     list? x) (  stringify-list x))
      ((   vector? x) (error "Converting vectors to strings is not supported (yet)"))
      ((procedure? x) (error "Converting procedures to strings is not supported (yet?)"))
      (else 
        (any->string:extension-handler x))))

