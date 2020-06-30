;;
;; Evaluation of a list (call)
;;

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

; There are two levels of matching:
;
; Syntax Match for forms
; Type   Match, for procedures
;
; (procedures are still forms;
; their arguments are all expressions.
; As convenience, all expressions are
; pre-evaluated before being passed
; to evaluation code

;
; TODO: now that the logic has been simplified dramatically,
; should we merge this file into some other file?
;

;
; TODO: how to implement exceptions in Scheme?
;
; Code will probably have a lot of:
; (cond
;   ((exception? x) x))
; after each call?
;
; ...either that, or we might add an explicit continuation argument
; to call in case of exception.
; 'scope nodes ex/c' ( = "exception-continuation" )
;
; ...are continuations tied to scopes? If so, we
; can re-use the 'scope' infrastructure?
;
; Can we re-use the '(return <value>)' pair approach
; to flow control?
;
; Think about it
;

;
; NOTE: 'resolve-as-scheme-callable' is used in files
; included before this one. It might be a good idea to
; move it to a different file which is loaded/included
; before the first use of the function.
;
(define (try-resolving-as-scheme-callable to-be-resolved)
  (cond
    ((basic-callable? to-be-resolved) (basic-callable->scheme-callable to-be-resolved))
    ((now-object? to-be-resolved) (try-resolving-as-scheme-callable (now-object-get-value to-be-resolved "__call__")))
    (else #f)))

(define (resolve-as-scheme-callable to-be-resolved)
  (let ((result (try-resolving-as-scheme-callable to-be-resolved)))
    (if result
      result
      (eval-error "Tried to call non-callable")))) ;; TODO: improve reporting, will probably result in an exception.

(define (execute-form scope-ref list-of-nodes)

  (define (resolve-form-head-as-scheme-callable scope-ref head-node)
    (resolve-as-scheme-callable (ast-node-eval (now.ref-deref scope-ref) head-node)))

  ;;
  ;; execute-form : Main body
  ;;
  (assert (list? list-of-nodes))
  (if (null? list-of-nodes)
        ;;
        ;; TODO: see how empty-list is handled in
        ;; now-terp, do something similar here?
        ;;
    now.none
        ;; What to return in case we get the empty node?
        ;; Should the empty node even be allowed?
        ;; (Maybe -- if executing {; ;; ;} ?).
        ;; 'now.none will be a placeholder return value
        ;; for now.
        ;;
        ;; Should we return sone sort of scheme-only
        ;; 'weak none singleton' that will have to be
        ;; interpreted differently depending on who
        ;; called this function?
        ;;
        ;; Could be the scheme list (empty), handled
        ;; using the same mechanism as of flow control?
        ;;
    (let ((scheme-proc-for-form (resolve-form-head-as-scheme-callable scope-ref (car list-of-nodes))))
      (scheme-proc-for-form scope-ref list-of-nodes))))

