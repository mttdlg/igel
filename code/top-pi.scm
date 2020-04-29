;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; IGEL                         ;;
;;                              ;;
;; Interpreter /                ;;
;; Generator for                ;;
;; Experimening with            ;;
;; Languages                    ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Top file, Platform Independent
;;
;; We assume that platform-specific
;; code has been run by now.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define extra-tests #t)
(define extra-tests #f)

(define (print-header h)
  (newline)
  (display h)
  (newline)
  (newline))

;;
;; Common, generic support
;;
(load-ct "top-common.scm")

;;
;; Base Types
;;
(load-ct "top-basetypes.scm")

;;
;; Syntax (text -> AST)
;;
(load-ct "top-syntax.scm")

;;
;; Semantics (AST Interpreter)
;;
(load-ct "top-semantics.scm")

;;
;; Test routines (to be replaced
;; with proper main loops later)
;;

;
; Replace 'if' with conditional expansion
; macro (look if there is a standard one
; in SRFI, otherwise write your own).
; change definitely needed for gambit,
; which doesn't like imports inside
; of if's.
; what about chicken?
;
; We will probably need one conditional
; expansion per test here, and one in
; the entry-point function?
;

; (if extra-tests
;   (begin
;     (load-ct "tests/test-tokens.scm")
;     (load-ct "tests/test-parser.scm")
;   )
; )

(load-ct "tests/test-now-terp.scm") ;; TODO: this should probably be incorporated into the main code?
(define (entry-point list-of-tests)
;  (print-header "Testing tokeniser")
;  (do-test-tokeniser list-of-tests)
;  (print-header "Testing parser")
;  (do-test-parser    list-of-tests)
;  (print-header "Testing now-interpreter")
  (do-test-now-terp  list-of-tests))

; Invoke a 'finalization' macro, which will be expanded depending on the specific interpreter.
; It will call the entry point in an appropriate way.

(top-pi-finalize)
