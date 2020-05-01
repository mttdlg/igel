;;
;; Test for 'inchain'
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


; (import (scheme small))

;; Guile?
;; (use-modules (srfi srfi-9))  ; define-record-type
;; (use-modules (srfi srfi-69)) ; make-hash-table
;; (load "../support.scm")
;; (load "../inchain.scm")

(load "support.scm")
(load "inchain.scm")

(define (display-whole-chain inchain)
  (let ((item (inchain-get-item inchain)))

    ;; TEST for line/column

    (if (eqv? item #\Q)
      (begin
        (newline)
        (display (location->string (inchain-get-location inchain)))
        (newline)))

    ;; Main loop (read whole file, get to EOF)

    (cond
      ((end-of-chain? item) #t)
      (else
        (display item)
        (let ((next-chain (inchain-get-next inchain)))
          (assert next-chain)
          (display-whole-chain next-chain))))))

(display-whole-chain (make-inchain-from-file "infile.txt"))

(newline)
(newline)
(display "--- Next phase ---")
(newline)
(newline)
(newline)

(let ((i (make-inchain-from-file "infile.txt")))
  (display "--- First run: port") (newline)
  (newline)

  (display-whole-chain i)

  (display "--- Second run: stored data structure") (newline)
  (newline)

  (display-whole-chain i))
