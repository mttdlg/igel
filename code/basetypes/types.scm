;;
;; Types
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
(define-record-type
  <type>
  (make-type name constructor typecheck)
  type?
  (name type-get-name)
  (constructor get-constructor)
  (typecheck type-get-typecheck))

;;
;; Thing that can be put into drawers
;; and used in expressions. Evaluation
;; should always return this (review
;; all code, change it to use this --
;; even 'void should probably be changed
;; to match this)
;;
;; NOTE: Void/Null/None should be a singleton
;;
;; ALso note: will now-object still need a KIND after we introduce this record?
;;

(define-record-type
  <now-typed-value>
  (make-now-typed-value type value)
  now-typed-value?
  (type  now-typed-value-get-type)
  (value now-typed-value-get-value))

;;
;; Support functions
;;

(define (check-type type item)
  ((type-get-typecheck type) item))
;;
;; Int
;;
(define type-Int? integer?)
(define (make-type-Int val)
  ;; TODO: replace 'assert' with 'ensure',
  ;; everywhere it makes sense.
  (assert (type-Int? val))
  val)
(define type-Int
  (make-type "Int" make-type-Int type-Int?))

;;
;; Uint
;;

; cannot do it yet, we need a way to ensure that
; mathematical operators behave appropriately.
; Does it make sense to have Uint?
; Should we have just Int?

;;
;; String
;;
(define type-String? string?)
(define (make-type-String val)
  (assert (type-String? val))
  val)
(define type-String
  (make-type "String" make-type-String type-String?))

;;
;; Support values/functions to facilitate refactoring
;; (will be upgraded to proper IGEL types at one point)
;;

(define (fresh-igel-empty-list)
  (list))

;;
;; These will be redefined
;; as igel-values later.
;; For now, we re-use
;; scheme booleans.
;;
(define now.true  #t)
(define now.false #f)

;;
;; none (singleton)
;;
;; Temporary implementation to begin
;; the refactoring process. 
;; Later on, these defines
;; will deal with proper igel-values.
;;
(define none-singleton 'none-singleton)
(define (none-singleton? maybe-none)
  (eq? maybe-none 'none-singleton))

;;
;; Callables
;;

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
    ;; Alternative approach:
    ;;
    ;; Non-infix: *true  # special characters do not need quoting
    ;; Infix:     \*true # special characters need quoting
    ;;
    ;; Will probably go with the latter.
    ;;
    ;; Also, possibly have a 'canonical representation' (depending on language) that one can hook into?
    (else
      (any->string now-val)))) ;; Convert scheme values to scheme strings.
;;
;; Types need to go in a namespace/scope.
;;
