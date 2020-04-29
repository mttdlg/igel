;;
;; Platform specific files for TinyScheme
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
;; TODO: the type system is just a very early, quick
;; and dirty draft, and very much a work in progress.
;; The contents of this file will probably have
;; to be thrown away (might be replaced by
;; different contents)
;;

;;
;; Each now-object will contain (or refer to) a 'type' record.
;;
;; Types are simple records, as opposed to full-fledged objects.
;; Otherwise we'd have a circular dependency.
;;
;; We /might/ provide now-objects which represent types, but
;; /Internally/, we do not use type-objects, we use 'raw' types.
;; But if code needs a 'type-object', we might construct one
;; (object of kind 'type', where the type-record is a value:)
;;

;; fake-srfi-69 at least requires 'assert'
(macro (assert s-expr) #t) ;; disable assertions

(load "./platform/generic/fake-srfi-1.scm")  ; list library (used at the moment?)
(load "./platform/tinyscheme/fake-srfi-9.scm")  ; define-record-type
(load "./platform/generic/fake-srfi-69.scm") ; hashes
; (load "../tinyscheme/srfi-69.scm") ;; From the official SRFI-69 document;
                                     ;; slower than the 'fake' version when
                                     ;; used with our small test files.


;;
;; TinyScheme does not have define-syntax.
;;
;; TODO: display the assertion
;; that has been violated.

;(define (assert p)
;  (if (not p) (error "Assertion violated")))

(define (file-exists? f)
  #t) ;; Bypass completely, just to make the code run
      ;; for bootstrapping purposes.
      ;;
      ;; TODO: implement more properly using catch/throw
      ;; (possibly opening and closing the file very quickly?)
      ;;
      ;; Or, perhaps... make all platforms rely on a custom
      ;; procedure (open-input-file-else filename error_message)

;;
;; To enable assertions, uncomment the following code
;; and comment out the functionally trivial definition
;; of 'assert' that follows.
;;
;(macro (assert s-expr)
;   (let ((args (cdr s-expr)))
;     (case (length args)
;       ((0)  (error "assert requires an argument!"))
;       ((1)  `(if (not ,(car args))
;                (begin
;                  (display " ---> ")
;                  (write (quote ,s-expr))
;                  (newline)
;                  (error "Assertion violated"))))
;       (else `(if (not ,(car args)) (begin ,@(cdr args)))))))

(define error-and-exit error)

(macro (load-ct args)
  `(load ,(cadr args)))

(macro (bind-list args)
  (let* ((proper-args (cdr args))
         (vars (car  proper-args))
         (vals (cadr proper-args))
         (body (cddr proper-args)))
    `(apply (lambda ,vars ,@body) ,vals)))

(define (top-pi-finalize)
  (entry-point 
    (if (defined? '*args*)
      *args*
      '() )))

;;
;; boxes
;;
; (load-ct "./platform/generic/generic-srfi-111.scm")

;;
;; Seems like only 'guile' has this,
;; tinyscheme and chicken don't?
;;
;(define (filter keep-it? l)
;  (let filter-loop ((l l)
;                    (result-r '()))
;    (if (null? l) (reverse result-r)
;      (let ((item (car l)))
;        (if (keep-it? item)
;          (filter-loop (cdr l) (cons item result-r))
;          (filter-loop (cdr l) result-r))))))
