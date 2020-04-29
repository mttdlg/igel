;;
;; Generic support functions that can be used anywhere,
;; not specific to any one module
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

;;
;; Normalise result when defining predicates:
;;
(define (any->boolean x)
  (if x #t #f))

;; Misc helper functions.

;;
;; Check that all elements of a list
;; satisfy a predicate.
;;
;; It is not specified how many times
;; the predicate will be applied, nor
;; to which elements (so do not use
;; the predicate for side effects).
;;
;; If one element fails the check,
;; return #f immediately, and do not apply
;; the predicate to the following ones.
;;
;(define (all-pass? check? l)
;  (cond
;    ((null? l) #t)
;    ((if (check? (car l))
;      (all-pass? check? (cdr l))
;      #f))))

;; assoc-ref: see if there's a way to define it
;; only if the implementation does not provide it
;; (some do, like gauche and guile.
;;  some do not, like chicken)

;; TODO: replace with hash?
;; Make it generic ('table'), so it works
;; with any implementation?
(define (assoc-ref alist key)
  (let ((entry (assoc key alist)))
    (if entry
      (cdr entry)
      #f)))

;;
;; Make result a boolean
;; (this will be used as part
;; of other predicates, which
;; are expected to return a
;; boolean, not a list)
;;
(define (member? elem l)
  (if (member elem l) #t #f))

(define (char->string c)
  (list->string (list c)))

;;
;; End of line: physical line terminator.
;; Will be used for incrementing line counter
;; in input files, for error reporting.
;;
;; Not to be confused with the syntactic
;; line terminator (';' for instance),
;; whose corresponding predicate is
;; "char-line-terminator?"
;;

(define (char-eol? c)
  (char=? c #\newline))

(define (key-value-pairs->hash-table list-of-key-value-pairs)
  ;;
  ;; Mainly used to define table-initialization
  ;; procedures, but can theoretically used anywhere.
  ;;
  ;; Why not use alist->hash-table directly?
  ;; Because alist->hash-table expect the value
  ;; to be the cdr of each sub-list, not the cadr.
  ;;
  ;; TODO: maybe we should get rid of this function and
  ;; define init-lists using the dot-notation instead.
  ;;
  (let ((hash-table (make-hash-table)))
    (for-each
      (lambda (l) 
        (assert (null? (cddr l))) ;; Check that it's a list with two elements
        (hash-table-set! hash-table (car l) (cadr l)))
      list-of-key-value-pairs)
    hash-table))
