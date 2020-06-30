;;
;; IGEL
;;
;; Definition of basic types
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
;; Thing that can be put into drawers
;; and used in expressions. Evaluation
;; should always return this (review
;; all code, change it to use this --
;; even 'void should probably be changed
;; to match this)
;;

;;
;; Every now-type is a sub-type of now.any.
;;
;; The current plan is to implement later-types
;; as specific now-types, so now.any is also
;; a super-type for later-types.
;;
;; This is used at least as a placeholder
;; while implementing the type system:
;; early draft code did not really typecheck,
;; and relied on scheme's dynamic typing
;; to reach a state where the basic architectural
;; points (mainly syntax and basic semantics
;; of command/functions) could reach the
;; proof-of-concept phase as quickly as possible.
;;
;; Now that the basic concepts have been clarified
;; and proven, it is time to move on to the next step:
;; proper typing.
;;
;; Introducing "any" will work as a stepping stone
;; for still allowing dynamic typing patterns during
;; a phase of heavy refactoring, where we migrate the
;; code to use the new type infrastructure without
;; changing the underlying algorithms.
;;
;; We will decide later on whether to keep now.any
;; in the final definition of the language or remove
;; it (still uncertain about what to do here, but currently
;; leaning /slightly/ towards removing now.any
;; eventually -- if the target language requires it, however,
;; it is still entierely possible to define a later.any).
;;
;; If we decide to remove now.any, we will then have a
;; second phase where 'dynamic typing' will be replaced
;; by stricter approaches.
;;

(define now.any
  (make-type "now.any"
             type-no-fallback-types
             type-no-config-check
             any->string ;; TODO: this is a first attempt, not much thought was put into making it 100% correct.
                         ;; Review and improve if necessary.
             #f ;; No default data constructor.
             ))

(define type-default-fallback-types (list now.any))

;;
;; now.type : an IGEL type for holding IGEL types as data.
;;
(define now.type
  (make-type "now.type"
             type-default-fallback-types
             type-no-config-check
             (lambda (t) (string-append "[now.type " (type-get-printable-name t) " ]")) ;; FIXME this is just a placeholder
             #f ;; No default data constructor
             ))
;;
;; now.none , singleton (replace none with now.nil?)
;; TODO: consider relationship between VOID (type), NONE (singleton), and NULL (reference)
;; VOID is the type for procedures which return nothing.
;; NONE will be the only object of the VOID type.
;; NULL will be a special value for references (each NULL
;; will be of the appropriate type -- it is not allowed
;; to assign a NULL of one type to a different type)
;;
(define now.none-data #f) ;; Value is irrelevant, but we want a canonical value.
(define (now.none-data? x) (not x))

(define now.void
  (make-type "now.void"
             type-default-fallback-types
             type-no-config-check
             (lambda (_) "now.none") ;; TODO: canonicize.
                                 ;; *none or `none or <none> when available?
             (constant->default-constructor now.none-data) ;; Value is irrelevant, but we have a canonical value.
             ))

(define now.void-type-id (type-get-unique-id now.void))

(define now.none (make-igel-value now.void now.none-data)) ; singleton

(define (now.none? x)
  (assert (igel-value? x))
  (let* ((type (igel-value-get-type x))
         (type-id (type-get-unique-id type)))
    (assert (now.none-data? (igel-value-get-data x))) ; self-check, actual assertion. Value is actually irrelevant in production, but this might catch bugs.
    (type-id=? type-id now.void-type-id)))

;;
;; now.int
;;
(define now.int
  (make-type "now.int"
             type-default-fallback-types
             type-no-config-check
             (lambda (value) ;; stringify
               (let ((data (igel-value-get-data value)))
                 (number->string data)))
             (constant->default-constructor 0)
             )) ;; TODO: improve

;;
;; For now, access to data is a bit blurred.
;; We guarrantee that now.int? will have no sub-classes
;; (but the separate now.Int type/class might), so if we
;; test for now.int? , we ensure we can also trust the
;; internal representation of data.
;;
(define (now.int? x)
  (same-type-id? now.int (igel-value-get-type x)))

(define (now.int->integer igel-now-int)
  (assert (now.int? igel-now-int))
  (igel-value-get-data igel-now-int))

;;
;; now.uint
;;

; cannot do it yet, we need a way to ensure that
; mathematical operators behave appropriately.
; Does it make sense to have Uint?
; Should we have just Int?


;;
;; bool
;;
(define now.bool
  (make-type "now.bool"
             type-default-fallback-types
             type-no-config-check
             (lambda (value)
               (if (igel-value-true? value) "true" "false")) ;; TODO: canonicize?
                                                             ;; *true, *false? `true, `false?
                                                             ;; 'TRUE, 'FALSE? <true> <false> ?
                                                             ;;
                                                             ;; [.now.bool 'TRUE], [.now.bool 'FALSE] is probably
                                                             ;; the most regular and least intrusive
                                                             ;; way to deal with it, even if not the most compact.
                                                             ;;
                                                             ;; The question remains, though: if the user overrides
                                                             ;; the 'root module' with chroot, how do we canonically
                                                             ;; represent these types? One of the desired feautres of
                                                             ;; IGEL is that internal representation should always be
                                                             ;; writeable in a way that can later be read back in.
                                                             ;;
                                                             ;; Perhaps this problem should be solved for custom classes
                                                             ;; first: that case is more complex and general, and once
                                                             ;; that one has been solved, the case of boolean values
                                                             ;; will probably be only a special case that can use
                                                             ;; the same solution.
             (constant->default-constructor #f)
             ))

(define now.true  (make-igel-value now.bool #t))
(define now.false (make-igel-value now.bool #f))
;;
;; Will evaluate an IGEL now.bool value
;; and convert it to a scheme boolean.
;; Will be used, for instance, in the
;; implementation of flow control
;; constructs.
;;
;; At the current stage in the implementation,
;; primitive IGEL types are mapped /directly/
;; to scheme values already, so no conversion
;; is necessary and the function is trivial.
;;
;; However, this will change in the near future.
;;
(define (igel-value-true? igel-bool)
  ; (assert (boolean? igel-bool))
  (assert (type-accepts-value? now.bool igel-bool)) ;; Later: allow for now.Exception
  (igel-value-get-data igel-bool)) ;; For now, we store #t and #f there.
                                   ;; If later on 'bool' becomes an
                                   ;; enumertated type, change this part.

;;
;; now.string
;;
(define now.string
  (make-type "now.string"
             type-default-fallback-types
             type-no-config-check
             (lambda (value)
               (let ((data (igel-value-get-data value)))
                 data))
             (constant->default-constructor "")
             ))

;;
;; now.symbol
;;
;; (First implementation, we re-use the underlying
;; scheme implementation. Later on we will need to
;; come up with our own string->id mechanism)
;;
(define now.symbol
  (make-type "now.symbol"
             type-default-fallback-types
             type-no-config-check
             (lambda (value)
               (let ((data (igel-value-get-data value)))
                 (string-append "'" (symbol->string data))))
             no-default-constructor ;; ...think of something?
             ))

(define (now.symbol? x)
  (type-accepts-value? now.symbol x))

(define (now.symbol=? s1 s2)
  (assert (now.symbol? s1))
  (assert (now.symbol? s2))
  (eq? (igel-value-get-data s1)
       (igel-value-get-data s2)))

;;
;; now.form
;;
(define now.form
  (make-type "now.form" 
             type-default-fallback-types
             type-config-match-placeholder ;; TODO/FIXME: it will need to take signature into account!
             (lambda (_) "<now.form>") ;; TODO: just a placeholder for now.
             no-default-constructor ;; TODO: replace with a null function? Or a function that returns the return type's default value?
             ))
