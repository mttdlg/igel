;;
;; Support for 'set' delegation.
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

;;
;; New version with support for delegation
;;
;; NOTE: delegation happens if and only if we pass an /evaluation/ as the first argument.
;;
;; For instance:
;;
;;  "set [ foo bar ] value"
;;  "set [ foo ] value"
;; 
;; If we pass an ID directly, not enclosed
;; in a list, like this:
;;
;;  set foo value
;;
;; ...then we do not perform delegation to the target objet!
;; What we are currently planning is delegating 'set'
;; to the appropriate entry in the scope/id table (or the
;; scope/id table itself -- to be defined).
;;
;; Strategy (rework code to make it fit) -> (look into "execute-form.scm" ?)
;;
;; * We *evaluate* head of 'invocation' list. *Must* be object.
;; * We look for a __set__ member
;;   * If available, we delegate everything to it
;;   * If not, we raise an error.
;; 

;;
;; Question: when do we evaluate
;; the VALUE to be assigned?
;; Before starting to resolve the chain,
;; or at the end of delegation?
;;
;; This is important. It will also affect
;; how proc/functions are written.
;;
;; Simple approach:
;; * target._set_ will receive the *unevaluated value*
;;   (syntax object) and will be responsible
;;   for deciding whether to evaluate or delegate it.
;;   Does this make the mechanism too complex?
;;   Is requiring an [eval value] too much?
;;
;; Other possibility:
;; * Attempt to delegate to _set_form_
;;   * _set_form_ will be responsible to
;;     delegate along the _set_form_ chain
;; * if _set_form_ is not available, 
;;   resolve and delegate to _set_
;;   * _set_ will delegate along the _set_ chain,
;;    but not along the _set_form_ chain?
;;
;; It probably makes sense to start with the first approach.
;; We might need it anyway in the final version, and it
;; is not too hard to adapt things later if we change
;; our mind.

(define (get-callable-for-now.set obj)
  (assert (now-object? obj)) ;; TODO: better error checking/reporting
  (if (now-object-has-member? obj "__set__")
      (resolve-as-scheme-callable (get-now-object-member obj "__set__")) ;; rawattr, like for '__dot__'
      (error (string-append "'set' operation cannot be delegated to object without __set__")))) ;; TODO: better error reporting

(define (set-using-invocation scope invocation-list-nodes value-node)
  (if (null? invocation-list-nodes)
    (error "Cannot have an empty call as first argument of 'set'!")
    (let* ((head-node (car invocation-list-nodes))
           (head-object (ast-node-eval scope head-node))
           (head-scheme-callable (get-callable-for-now.set head-object))) ; delegates raising an error if not available.
      (head-scheme-callable scope
                            (list head-node
                                  (make-ast-node 'list (cdr invocation-list-nodes))
                                  value-node)))))

(define (set-id-string-to-value scope id-string value)
  (let* ((drawer (resolve-id-to-drawer-or-complain scope id-string))
         (setter (drawer-get-setter drawer)))
    (setter value)
    none-singleton)) ;; Should this return none-singleton ? Should it return the passed value ?

