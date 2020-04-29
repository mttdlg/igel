;;
;; Files common to many (all?) platforms
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
;; providing it for implementations
;; that do not have it?
;; Either that, or proveide a 
;; workaround like a procedure/form named
;; 'assert-file-exists' ?
;;

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

(define-syntax assert
  (syntax-rules ()
    ((_ condition) 
     (if (not condition)
       (error "Assertion violated")))
    ((_ condition if-false ...)
     (if (not condition) (begin if-false ...)))))

;;
;; A 'load' of sorts that happens at compile time
;; if such a thing is available at all, otherwise
;; fall back to loading at run time.
;;
;; Implemented as 'include' in chicken and gambit.
;; TODO: see if tehre is a standard way to do this.
;;

(define-syntax load-ct
  (syntax-rules ()
    ((_ file-name) 
     (load file-name))))

;;
;; Chicken provides 'print',
;; Guile does not.
;;
;; TODO: double-check whether or
;; not chicken's version
;; adds a newline at the end.
;;
(define-syntax print
  (syntax-rules ()
    ((_ item ...)
     (begin
       (for-each display (list item ...))
       (newline)))))

