;;
;; IGEL
;;
;; now.ref : References
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
;; Placeholder for references.
;;

;;
;; Comparison with Specman/e :
;;
;; unit is instance -> Unit
;; unit             -> [ref Unit]
;; struct           -> [ref Struct]
;;

;;
;; Misc support definitions
;;

(define now.ref-type-id (gen-type-id))

;
; null refs
;

; What to store in the data field of a null reference.
(define null-ref-data #f)

; check whether or not a data field is a null reference
; (current hypothesis: all ref types store the same data
;  for a null reference)

(define (null-ref-data? x) (not x))

(define (ref? x)
  ;;
  ;; NOTE / TODO / FIXME :
  ;;
  ;; Right now we check EXACTLY for the ref type ID (since we
  ;; have too many changes to make just to get the code to run
  ;; again after implementing the new type sysetm).
  ;;
  ;; Once the parametric type infrastructure has been improved
  ;; in such a way that handling subclasses is simpler/cleaner,
  ;; make sure you also check for possible subclasses of ref
  ;; in the following code:
  (type-id=? now.ref-type-id
             (type-get-unique-id (igel-value-get-type x))))

(define (null-ref? x)
  (and (ref? x) (null-ref-data? (igel-value-get-data x))))

(define (now.ref type)
  (define (now.ref-accepts-config? config-accepted config-offered)
    (type-accepts-type?
      (drawer-table-get-drawer config-accepted "type")
      (drawer-table-get-drawer config-offered  "type")))
  ;
  ; Main body
  ;
    (make-parametric-type
      "now.ref"
      type-default-fallback-types
      now.ref-type-id
      now.ref-accepts-config?
      (lambda (_) "<placeholder string for ref>") ;; TODO/FIXME: implement properly
      (constant->default-constructor null-ref-data)   ;; Default DATA constructor. We assume that all types share the same data for NULL.
      (lambda (type) ; type-state-init
        ;; TODO: create idtable instead.
        (make-idtable-simple-from-data #t
          `((const "type" ,now.type ,type))))))


;;
;; To be used for internal code, at least for a first implementation.
;; Later on it should be removed and replaced with something cleaner.
;;
(define (ref-valid? type value)
  (or (null-ref-data? value)
      (type-accepts-value? type value)))

(define (now.ref-from-type-and-value type value)
  ; First implementation: we cheat, and create then overwrite
  ; a reference. In the future, we need to set up a proper
  ; constructor for references.
  (assert (ref-valid? type value))
  (let* ((tmp-ref (now.ref type)))
    (igel-value-set-data! tmp-ref value)))

(define (now.ref-deref-nochecks ref)
  (igel-value-get-data ref))

(define now.ref-deref now.ref-deref-nochecks) ; first, quick-and-dirty implementation. TODO: add checks.
;;
;; What type is the constant NULL?
;;
;; Current plan: foresee some sort of casting mechanism.
;; The symbol NULL will be an object of its own type, a type
;; which will almost certainly not be a reference ('void'
;; or 'nulltype'?), or AnyReference at most (if we decide to have
;; an AnyReference type). It will, however, be possible to
;; safely cast NULL to any reference type.
;;
;; This way, a null reference stored into a variable of type X
;; will actually be an object of type [ref X], distinct (but
;; possibly cast'able as) an object of type [ref Y].
;;
;; Another way to see it: if we had various types for numbers
;; (integer, complex, fractional, real), the 'NULL' reference
;; would be handled in a manner similar to how we would handle
;; the number 0.
;;
;; SO:
;;
;; - first draft: NULL is not a constant; One obtains a null reference
;;   of the correct type with the default constructor [ref object].
;;   Alternately: [ref -null object] as syntactic sugar?
;;   Check with [null? r]
;;
;; - later: foresee a generic safe-casting mechanism for values (for instance,
;;   it might be safe to assign appropriately sized integer constants to
;;   [Bits] or [Mvl]). NULL could be handled as part of this.
;;
