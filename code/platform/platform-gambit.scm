;;
;; Gambit-specific setup
;;
;; Copyright 2020 Matteo De Luigi
;;

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

;;
;; Code uses 'file-exists?', which
;; technically is R6RS. See about
;; implementing it for implementations
;; that do not have it?
;; Either that, or proveide a 
;; workaround like a procedure/form named
;; 'assert-file-exists' ?
;;

;;
;; LOAD at Compile Time
;;
;; A 'load' of sorts that happens at compile time
;; if such a thing is available at all, otherwise
;; fall back to loading at run time.
;;
;; Implemented as 'include' in chicken.
;; TODO: see if tehre is a standard way to do this.
;;

(define-macro (load-ct file-name)
  `(include ,file-name))

;; fake-sfri-69.scm, below, uses 'assert', so
;; we define it before loading it.
(define-macro (assert condition . if-false)
  (if (null? if-false)
    `(if (not ,condition) (error "Assertion violated"))
    `(if (not ,condition) (begin ,if-false ...))))

; (define-macro (assert condition . if-false) #t)

; Available by default: srfi-9, records
(include "generic/fake-srfi-1.scm")  ; list library (used at the moment?)
(include "generic/fake-srfi-69.scm") ; hashes

;; (define main . argv) ;; ?
;; TODO: Quick placeholder, look up proper definition?
(define error-and-exit error)

(define-macro (bind-list vars vals . body)
   ; (define (main . argv) (entry-point (cdr argv)) )
  `(apply (lambda ,vars ,@body) ,vals))
;;
;; For schemes that do not define it
;; (Chicken does, but it is different. Chibi-scheme's 'scheme-small' does not.)
;;
;; TODO: display the assertion
;; that has been violated.
;;
;; TODO: define semantics clearly. Can assertions go away in optimised code?
;; Are they always there? In this case... should the procedure be renamed
;; to "ensure"?
;;
; (define-library (igel misc)
;  (export assert)
;;
;; TODO: improve macro to actually print (write?) the expression that failed
;;
;; TODO: separate 'assert' (which could theoretically be removed in a
;; non-debug build) and 'check' (which must /always/ be there)


(define-macro (top-pi-finalize)
   ; (define (main . argv) (entry-point (cdr argv)) )
  `(entry-point (cdr (command-line))))
; (define (top-pi-finalize) (entry-point (cdr (command-line))))
