;;
;; Fake SRFI-69
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
;; Provide barely enough an approximate implementation
;; of the parts of SRFI-69 used in our code
;; to get the rest of the code working.
;; Somehow.
;;

(define (make-hash-table) (list 'fake-hash))

(define (hash-table-ref table key)
  (assert (eq? (car table) 'fake-hash))
  (let* ((alist (cdr table))
         (outcome-of-lookup (assoc key alist)))
    (if (pair? outcome-of-lookup)
         (cdr (assoc key alist))
         (error "Fake-69: lookup failed!"))))

(define (hash-table-exists? table key)
  (assert (eq? (car table) 'fake-hash))
  (let* ((alist (cdr table))
         (pair-if-exists (assoc key alist)))
     (if pair-if-exists #t #f)))

(define (hash-table-set! table key value)
  (assert (eq? (car table) 'fake-hash))
  (let* ((alist (cdr table))
         (pair-if-exists (assoc key alist)))
    (if pair-if-exists
      (set-cdr! pair-if-exists value)
      (set-cdr! table
                (cons (cons key value)
                      alist)))))

(define (alist->hash-table alist)
  (cons 'fake-hash alist))
