;;
;; IGEL
;;
;; Support values/functions to facilitate refactoring
;; (will be upgraded to proper IGEL types at one point)
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

(define (fresh-igel-empty-list) (list)) ;; NOTE: eq? guarrantees #t on empty lists.

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; At the moment, a basic-callable /is/ a scheme procedure.
;; This will change in the future.
(define basic-callable? procedure?)
(define (basic-callable->scheme-callable basic-callable)
  basic-callable)

;;
;; Misc support
;;
(define (now-val->string now-val)
  ;; Converts scheme values to target now-language strings.
  ;; Very simple implementation for now:
  (cond
    ((boolean? now-val) (if now-val "true" "false")) ;; TODO: should there be non-redefinable symbols for this case? Our concept is that there are no reservd identifiers /a priori/ (we can add our own reserved words to domain specific languages, but the set starts out EMPTY). Because, if we redefine the symbols 'true' and 'false', the reported strings would no longer be accurate...
    ;; Perhaps define a special parenthesization for non-redefinables,
    ;; like [:true:] and [:false:]
    ;; or $true $false
    ;; or `true `false
    ;; (try not to conflict with C-style expressions -- or *true *false would be nice, but might be problematic in infix notation)
    ;; Make spaces mandatory in infix notation?
    ;;
    ;; (a + b) -> [add a b] # expression
    ;; (a+b) -> a+b # identifier
    ;;
    ;; (1 *true) -> 1 *true # number followed by ID
    ;; (1 * true) -> [mul 1 true] # Maybe valid in a language where 'true' is not a reserved word?
    ;; 
    ;; Non-infix: *true  # special characters do not need quoting
    ;; Infix:     \*true # special characters need quoting
    ;; Also, possibly have a 'canonical representation' (depending on language) that one can hook into?
    (else
      (any->string now-val)))) ;; Convert scheme values to scheme strings.
;;
;; Types need to go in a namespace/scope.
;;
