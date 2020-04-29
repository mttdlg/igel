;;
;; Chicken-specific setup
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
;; LOAD at Compile Time
;;
;; A 'load' of sorts that happens at compile time
;; if such a thing is available at all, otherwise
;; fall back to loading at run time.
;;
;; For chicken, we want the files to be
;; compiled into the executable, not
;; loaded at runtime.
;;

(define-syntax load-ct
  (syntax-rules ()
    ((_ file-name) 
     (include file-name))))

(require-extension srfi-1)  ; list library (used at the moment?)
(require-extension srfi-69) ; hashes

(define error-and-exit error)

(define-syntax bind-list
  (syntax-rules ()
    ((_ vars vals body ...)
      (apply (lambda vars body ...) vals))))

;;
;; Assertions,
;; For schemes that do not define it
;; (Chicken does, but it is different. Chibi-scheme's 'scheme-small' does not.)
;;
;; TODO: display the assertion
;; that has been violated.
;;
;; TODO: change most usages of 'assert' in actual code
;; with 'expect' or something else which can NOT be
;; disabled (where appropriate)
;;
; (define-library (igel misc)
;  (export assert)

;; Chicken has 'assert', but it is different from ours...

;(define-syntax assert
;  (syntax-rules ()
;    ((_ c msg)
;     (if (not c) (error msg)))
;    ((_ c)
;     (if (not c) (error "Assertion violated!")))))

;; disabled assert:
; (define-syntax assert
;   (syntax-rules ()
;     ((_ c msg) #f)
;     ((_ c) #f)))

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

(define (top-pi-finalize)
  (entry-point (command-line-arguments)))
