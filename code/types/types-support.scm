;;
;; IGEL
;;
;; Groundwork for type support
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

;
; The first version of the type system will be
; a classic inheritance-based system, either
; with or without a base class for every possible
; type (first draft will probably not have it).
;
; Work on the experimental 'certification-based'
; concept will be postponed to after completing
; the more 'classic' approach just described.
;

;;
;; Type ID API start
;;

;
; In this section we define the interface for
; dealing with type-id's. This will allow us
; to change the implementation details later
; on, if needed (for instance, switching to
; using strings or records instead of
; integers).
;

(define gen-type-id (unique-id-gen-factory))
(define type-id=? =)

;;
;; Type ID API end
;;

; At the moment, the type has only a 'state'
; In the future, we plan to have a 'config',
; determined at item creation time and never
; changed.
; For things like data members which are part
; of the class and not of the instance (for
; example, the number times the class constructor
; has been called), we might place a reference
; in the config?  Do we still need a state?

; the name of the record constructor ends in '-private' because
; it is not part of the public type API.
; This record constructor is to be used only in the /implementation/
; internals of the public type API.

(define-record-type
  <type>
  (make-type-private unique-id printable-name fallback-types accepts-config? stringify-value default-data-constructor state)
  type?
  (unique-id        type-get-unique-id)
  (printable-name   type-get-printable-name)
  (fallback-types   type-get-fallback-types) ; 'parent classes', pretty much 
  (accepts-config?  type-get-accepts-config?)  ; compare configurations.
  (stringify-value  type-get-stringify-value)
  (default-data-constructor          ; Takes type as argument, to access configuration/parameters and update
                    type-get-default-data-constructor) ;; class attributes if necessary.
  (state            type-get-state)) ; For classes, it will contain class methods, class attributes, etc.
                                     ; If a type is parametric (for instance, array of type <t> and size <n>),
                                     ; At the moment, we also store the arguments in here, possibly as part of
                                     ; a compound data structure. -- In the future, we will need a separate
                                     ; 'config' field for the record.
;
; At the moment we do not support implicit/safe casting.
; We will need it at some point in the future (for
; enum/symbol-table types)
;

;;
;; Default/convenience definitions:
;;

;
; For types that have no state,
; this is the value we pass as state:
;
(define type-no-state #f) ;; Placeholder object, should never
                          ;; be accessed if the type does not
                          ;; rely on a type-state at all.
(define type-no-fallback-types '())
(define type-no-config-check (lambda (_1 _2) #t))
;;
;; Value for producing a constructor that always
;; returns the same scheme constant
;;
(define (constant->default-constructor k)
  (lambda (type) k))

(define (type-config-match-placeholder _)
  (error "Placeholder for a better type check, coming soon!")) ;; TODO: replace with ad-hoc exception.

(define (no-default-constructor _)
  (error "Attempted to call a default constructor for a type that does not support it!")) ;; TODO: replace with ad-hoc exception.

;
; For simple types:
;
(define (make-type printable-name fallback-types accepts-config? stringify-value default-data-constructor . extra-args)
  (let* ((state-as-list (default-arguments extra-args (list type-no-state)))
         (state (car state-as-list)))
    (make-type-private (gen-type-id)
                       printable-name
                       fallback-types
                       accepts-config?
                       stringify-value
                       default-data-constructor
                       state)))

;;
;; For parametric types
;; (for instance, an array takes a size and the type of its contents as parameters/arguments)
;;
;
; type-state-constructor :
;
;   returns a a procedure which takes any number of arguments as input
;   ('type-creation-arguments', passed as a list), and returns the parameter-specialized
;   type as output. The procedure which does the actual work of initializing the state
;   (also responsible for checking its arguments) is 'type-state-init', provided as
;   an argument to type-state-constructor.
;
; 'state' refers to the state of /type record itself/, not for the data of this type.
;

;
; TODO: instead of defining a LAMBDA, return a record 
; containing all relevant data members and methods associated
; with the type (type-id will be generated automatically and
; returned, for instance)
;
(define (make-parametric-type printable-name
                              fallback-types
                              type-id           ; same type ID as we change parameters
                              accepts-config?
                              stringify-value
                              default-data-constructor
                              type-state-init)

    (lambda type-creation-arguments
      (make-type-private type-id
                         printable-name
                         fallback-types
                         accepts-config?
                         stringify-value
                         default-data-constructor
                         (apply type-state-init type-creation-arguments))))
;;
;; IGEL value
;;
;
; Consists of 'type' (a <type> record) and
; 'data' (can be any scheme value, the <type> record
; will contain the information on how to deal with it).
;
(define-record-type
  <igel-value>
  (make-igel-value type data)
  igel-value?
  (type igel-value-get-type igel-value-set-type!)
  (data igel-value-get-data igel-value-set-data!))

;;
;; Type Compatibility infrastructure
;;

;
; Current algorithm for type compatibility:
;
; If we:
;   * expect type A (expected type) and
;   * we are offered type B (offered type)
;
; Phase I: deterimine compatibility of type IDs
;   - check if type ID matches exactly
;   - if so, Phase I is a success. Move on to phase II.
;   - if not, recursively check all types in the
;     'fallback-types fields of type B (offered).
;     Think of them as 'base classes' of sorts.
;     - If at least one of the fallback-types matches,
;       Phase I is a success. Move to Phase II.
;     - If not, we fail. Do not move on to Phase II.
;       Return false.
;
; Phase II
;   - call 'accepts-config?' of Type A (expected) on Type B (offered).
;     (accepts-config? will take both the "type A" object and the "type B"
;     objects -- it might want to compare the values of some fields, so it
;     needs both).
;     The result of this step is the result of the whole process. We return it.
;

(define (same-type-id? type1 type2)

  ; NOTE: same-type-id? does /not/ check
  ; that parameters are types. The caller
  ; is responsible for ensuring that.

  (type-id=? (type-get-unique-id type1) (type-get-unique-id type2)))

(define (type-id-compatible? type-expected type-offered)
  (if (same-type-id? type-expected type-offered)
    #t
    (let check-fallbacks ((fallback-types (type-get-fallback-types type-offered)))
      (if (null? fallback-types)
        #f
        (if (type-id-compatible? type-expected (car fallback-types))
          #t
          (check-fallbacks (cdr fallback-types)))))))


(define (type-accepts-type? type-expected type-offered) 
  (cond
    ((type-id-compatible? type-expected type-offered)
     (let ((expected-accepts-config? (type-get-accepts-config? type-expected)))
       (expected-accepts-config? type-expected type-offered)))
    (else #f)))

(define (type-accepts-value? type-accepted value-offered)
  (type-accepts-type? type-accepted
                      (igel-value-get-type value-offered)))

;
; 'drawers' (defined later) will need a typecheck
; based on a type. We provide the following 
; convenience function:
;
(define (make-check-for-type-from-type type-accepted)
  (lambda (type-offered)
    (type-accepts-type? type-accepted type-offered)))

(define (make-check-for-value-from-type type-accepted)
  (lambda (value-offered)
    (type-accepts-type? type-accepted (igel-value-get-type value-offered))))
