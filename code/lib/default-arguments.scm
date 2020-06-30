;;
;; Default arguments
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
;; This library handles the logic for a function which takes a number
;; of optional arguments, filling in the missing arguments with defaults.
;;
;; Example:
;;
;; (lambda ( a b c . extra-args )
;;   ...)
;;

;; try-default-arguments :
;;      Returns a list if successful, #f otherwise.
;;      At the moment, the only possible cause of failure
;;      is when we pass more arguments than defaults.
;;      This procedure is expected to be R5RS-compliant.
;;
;;
;; default-arguments :
;;      Depends on the 'error' procedure being available
;;      'error' is not part of R5RS (but is defined in SRFI-23)
;;

(define (try-default-arguments maybe-args default-values)
  (let default-arguments-helper
    ((maybe-args maybe-args)
     (default-values default-values)
     (partial-result-r '()))

    (cond
      ((null? default-values)
       (if (null? maybe-args)
         (reverse partial-result-r)
         #f)) ;; TODO: allow extra arguments without default values?
      ((null? maybe-args)
        (default-arguments-helper maybe-args
                                  (cdr default-values)
                                  (cons (car default-values)
                                        partial-result-r)))
      (else
        (default-arguments-helper (cdr maybe-args)
                                  (cdr default-values)
                                  (cons (car maybe-args)
                                        partial-result-r))))))

(define (default-arguments maybe-args default-values)
  (let ((result (try-default-arguments maybe-args default-values)))
    (if result
      result
      (error "Too many arguments passed, not enough defaults!"))))

;; (display (default-arguments '(a b c d) '(1 2 3 4))) (newline)
;; (display (default-arguments '(a b c) '(1 2 3 4))) (newline)
;; (display (default-arguments '(a) '(1 2 3 4))) (newline)
;; (display (default-arguments '()  '(1 2 3 4))) (newline)
;; (display (default-arguments '(a b c d e) '(1 2 3 4))) (newline)
