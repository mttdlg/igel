;;
;; Evaluation logic
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

(define eval-error error-and-exit)

; (define (ast-node-leval-exp scope node)
;  ;; Expression must evaluate to an lvalue,
;  ;; which we return (as a drawer)
;  (error "ast-node-leval-exp: unimplemented"))

(define (ast-node-eval-exp scope node)
  (execute-form
    scope
    (ast-node-value-from-kind node 'list)))

;;
;; 'Expand all' means 'expand all levels of macros',
;; i.e., keep expanding until we have something which
;; is considered a fully expanded form.
;; Maybe, instead of looping here, it will make sense
;; to delegate detection of base step to the expansion
;; procedure we call...
;; In that case, decide how to implement 'expand-once',
;; though.
;;
(define (ast-node-expand-all-and-eval scope node)
  (let ((result (ast-node-eval-exp scope node)))
    (if (expanded-form? result)
      ; Then-branch is still untested because we do not
      ; generate expanded forms yet:
      (ast-node-eval scope (expanded-form-get-node result))
      result)))

;;
;; Do we need to distinguish
;; between proc-eval and fn-eval?
;; Or do we make proc-eval pretty much
;; the same as fn-eval, only we expect it
;; to return a different type?
;;
;; What would the type of 'if' would be?
;; ...I suppose:
;;
;; if <a> then <b0> else <b1>
;;
;; <a> -> common_type(<b0>, <b1>)
;;
;; So, if Void is required,
;; then both <b0> and <b1>
;; must be of <void> type?
;;
;; But wait, what if there is
;; a "return <n>" in an if/then/else?
;;
(define (ast-node-eval scope node)
  ; (assert (ast-node? node))
  (case (ast-node-kind node)
    ;; For now, we handle only a subset
    ((id)         (now-get-drawer scope (ast-node-value node)))
    ((string int) (ast-node-value node))
    ((list)       (ast-node-expand-all-and-eval scope node)) ;; Defined elsewhere,
                                                ;; it might call into ast-node-eval
                                                ;; as a form of mutual recursion.
    (else => (lambda (unhandled-node-kind)
               (eval-error
                 (string-append
                   "Unhandled node kind in ast-node-eval: "
                   (symbol->string unhandled-node-kind)))))))

