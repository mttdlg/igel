;;
;; IGEL
;;
;; The "drawer" data structure
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
;; A "drawer" is an entry for ID tables
;;
;; Should 'drawer' be renamed to 'slot', or 'lval'?
;; Only possible risk: name 'lval' might be
;; needed by target language.
;; In this case, call it a "now.lval", in a manner
;; similar to how we have now.int, etc.?
;; maybe 'id-entry' for ID Table Entry

;;
;; NOTE: drawer must ALWAYS contain an IGEL VALUE
;; (which is a record type). Some contents-accessing
;; functions will return #f as a separate value to 
;; mean that it was not possible to access the contents,
;; so #f cannot be considered valid contents for a drawer.
;;

;
; TODO: rename 'setter' to 'setter!', and 'drawer-get-setter' to 'drawer-get-setter!-' ?
;
(define-record-type
  <drawer>
  (make-now-drawer  value-valid? getter setter)
  drawer?
  ;; Current approach: we do not specify an
  ;; associate type, just a gating predicate.
  ;; Maybe we will change this in the future,
  ;; but probably not. It would be interesting
  ;; to explore a 'by contract' kind of style.
  (value-valid? drawer-get-value-valid?-) ;; final '-' is to make it explicit that name does not
                                          ;; end in ?, therefore this function is not a predicate.
  (getter       drawer-get-getter drawer-set-getter!)
  (setter       drawer-get-setter drawer-set-setter!)) ;; To allow locking a drawer.

;;
;; Helper functions to create drawers for (now-)constants and (now-)variables
;; Should the functions be renamed from make-now-driver-... to make-driver-now-.... ?
;;

(define (make-now-drawer-const value-valid? contents)
  ;; defines go first
  (define (getter) contents)
  (define (setter new-value)
    (error "Attempted to assign a value to a now-constant"))

  ;; main body, now we can have assertions/checks
  (assert (value-valid? contents)) ;; TODO: support now.Exception
  (make-now-drawer value-valid? getter setter))

;;
;; TODO/FIXME: unify interface between CONST and VAR drawers?
;;
(define (make-now-drawer-var value-valid? contents)
  ;; defines go first
  (define (getter) contents)
  (define (setter new-value)
    (assert (value-valid? new-value)
                             ;; Assertion is just a fallback safety net.
                             ;; Later on we might do it here and raise an exception
                             ;; in case of failure? Gotta resolve the chicken-and-egg
                             ;; problem with defining Exceptions before drawers, though.
                             ;; If not, we might return a special value and delegate
                             ;; checks to caller
    (set! contents new-value)))

  ;; main body, now we can have assertions/checks
  (assert (value-valid? contents)) ;; support now.Exception, later.
  (make-now-drawer value-valid? getter setter))

;;
;; Generic function
;;
(define (make-now-drawer-simple access-policy drawer-type value)
  (let ((selected-make-now-drawer
          (case access-policy
            ((const) make-now-drawer-const)
            ((var)   make-now-drawer-var)
            (else => (lambda (p)
                   (error (string-append "Unhandled access-policy when making simple drawer: " p)))))))
    (selected-make-now-drawer (make-check-for-value-from-type drawer-type) value)))

;;
;; Simplified access
;;

(define (get-drawer-contents drawer)
  ((drawer-get-getter drawer))) ;; Notice the double level of parenthesis.

(define (set-drawer-contents drawer value)
  ((drawer-get-setter drawer) value))

