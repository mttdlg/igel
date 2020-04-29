;;
;; Support functions for forms
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
;; Define a special object with its own predicate:
;;
(define-record-type
  <expanded-form>
  (make-expanded-form node)
  expanded-form?
  (node expanded-form-get-node))

;;
;; Check number of arguments passed to a form.
;; TODO: make it mandatory to specify a name,
;; or, due to the 'dottable' concept,
;; improve the error message printed.
;;
(define (check-arg-count n nodes . form-name-list)
  (let ((form-id
          (case (length form-name-list)
            ((0) "<unknown>")
            ((1) (car form-name-list))
            (else
              (error "Too many arguments to check-arg-count")))))
  (if (not (= (length nodes) n))
    (eval-error (string-append "Wrong number of arguments when evaluating form '" form-id "'")))))

;;
;; Support function to check for "fixed words" like
;; 'from', 'to', 'then', 'else'.
;;
(define (node-n-id=? n nodes expected-id-string form-name)
  (let ((node (list-ref nodes n)))
    ;; Dottables?
    (if (ast-node-kind-match? node 'id)
      (string=? (ast-node-value node) expected-id-string)
      #f)))

; (define (process-nodelist nodelist fn rules)
;   (let process-nodelist-helper
;     ((nodelist nodelist)
;      (rules rules)
;      (done-r '()))
;     (cond
;       ((null? nodelist)
;        (if (null? rules)
;          (apply fn (reverse done-r))
;          (eval-error "Not enough nodes for all rules")))
;       ((null? rules)
;        (eval-error "Not enough rules for all nodes"))
;       (else
;         (process-nodelist-helper
;           (cdr nodelist)
;           (cdr rules)
;           (cons ((car rules) (car nodelist)) done-r))))))
