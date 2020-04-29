;;
;; Guile-specific setup
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
; (use-modules (srfi srfi-1))  ; list library (including 'drop' and 'filter'); is it used at the moment?
(use-modules (srfi srfi-9))  ; define-record-type
(use-modules (srfi srfi-69)) ; hashes
; (use-modules (srfi srfi-111)); box --> platform-generic redefines it

(define (error-and-exit msg)
  (let ((cep (current-error-port)))
    (display "*** Error: " cep)
    (display msg cep)
    (display " ***" cep)
    (newline cep)
    (exit 1)))

(define-macro (bind-list vars vals . body)
   ; (define (main . argv) (entry-point (cdr argv)) )
  `(apply (lambda ,vars ,@body) ,vals))

;;
;; Define global variable containing command line arguments
;;
(define (top-pi-finalize)
  (entry-point (cdr (program-arguments))))

;;
;; Load generic definitions
;;
(load "platform-generic.scm")
