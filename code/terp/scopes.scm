;;
;; Scopes
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

;; Should 'drawer be renamed to 'reference'? NO! It's a TABLE ENTRY!
;; maybe 'id-entry' for ID Table Entry
;; getter/setter -> deref, overwrite

;; TODO: re-use some other kind of
;; error-function?
(define scope-error error-and-exit)

;;
;; First step: define a "scope" object, chainable.
;;

(define now.Scope
  (make-now-class
    "now.Scope"
    (list now.Object)))

;
; TODO: the following three definitions need to be cleaned up.
; Right now the approach is very hackish/quick-and-dirty.
;
(define type-now.ref-to-now.Scope (now.ref now.Scope))

(define null-scope-ref (make-igel-value type-now.ref-to-now.Scope
                                        null-ref-data))

(define (null-scope-ref? scope-ref)
  (assert (type-accepts-value? type-now.ref-to-now.Scope scope-ref))
  (null-ref? scope-ref))
;
; Make a new sub scope.
;
(define (make-sub-scope ref-to-parent-scope)
  (make-igel-value
    now.Scope
    (make-now-drawer-table-simple-from-data #t
      `((const "id-table" ,now.idtable ,(make-fresh-idtable))
        (const "parent"   ,type-now.ref-to-now.Scope ,ref-to-parent-scope)))))
; Add an "on-exit" field, later?

(define (get-scope-id-table scope)
  (now-object-get-value scope "id-table"))

(define (get-scope-parent scope)
  (now-object-get-value scope "parent"))

;;
;; Top scope is pretty much like any other scope,
;; except its 'parent' scope is the null-scope-ref (null ref).
;;
(define top-scope (make-sub-scope null-scope-ref))

; ;;
; ;; scope-extract-drawer looks only in the
; ;; scope passed as argument.
; ;; It will not search parent/ancestor scopes
; ;; if the key is not found in the scope
; ;; passed as argument.
; ;;
; ;; TODO: split off try-extracting-drawer
; ;;
; (define (scope-extract-drawer scope name)
;   (let ((scope-table (get-scope-table scope)))
;     (if (hash-table-exists? scope-table name)
;       (hash-table-ref scope-table name)
;       (scope-error (string-append "Identifier not in scope table: " name)))))

;;
;; Support functions which work with drawers:
;;
;; TODO: add new function(s)) that
;; take ID NODE instead of NAME?
;;
;; TODO: add something like '-rs' to the function names
;; to make it clear that they take references to scopes?
;;
(define (try-resolving-drawer scope-ref name)
  (if (null-scope-ref? scope-ref)
    #f
    (let* ((scope (now.ref-deref-nochecks scope-ref))
           (scope-table (get-scope-id-table scope))
           (maybe-value (idtable-try-getting-value scope-table name)))
      (if maybe-value
        maybe-value
        (try-resolving-drawer (get-scope-parent scope) name)))))

;;
;; TODO: try-adding-drawer-to-scope
;;
(define (add-drawer-to-scope scope-ref name drawer)
  ;; TODO: Check for existence first!
  (assert (not (null-ref? scope-ref)))
  (let* ((scope (now.ref-deref-nochecks scope-ref))
         (scope-table (get-scope-id-table scope)))
    ; (write scope-table)
    ; (newline)
    (if (idtable-key-exists? scope-table name)
      (scope-error (string-append "Attempting to add an already"
                                  " existing id to scope: '"
                                  name "'"))
      (idtable-add-new-drawer! scope-table name drawer))))

;;
;; Creating drawers:
;;

(define (now-add-drawer scope-ref name kind value)
  (add-drawer-to-scope
    scope-ref
    name
    (make-now-drawer-var kind value)))

(define (now-add-const scope-ref name kind value)
  (add-drawer-to-scope
    scope-ref
    name
    (make-now-drawer-const kind value)))

; (define (resolve-id-to-drawer-or-complain scope-ref name)
(define (resolve-id-to-drawer scope-ref name)
  (let ((maybe-drawer (try-resolving-drawer scope-ref name)))
    (if maybe-drawer
      maybe-drawer
      (scope-error (string-append
                     "Could not resolve identifier: '"
                     name
                     "'")))))

;;
;; TODO: the names of these functions are very old.
;; come up with something more fitting the current
;; coding conventions.
;;
;; now-get-drawer -> scope-get-drawer (and scope-try-getting-drawer)
;; now-set-drawer -> scope-set-drawer-value!
(define (scope-get-drawer scope-ref name)
  (let ((drawer (resolve-id-to-drawer scope-ref name)))
    (get-drawer-contents drawer)))

;
; ; Not used anymore?
;
; (define (now-set-drawer-value scope name value)
;  (let ((drawer (resolve-id-to-drawer-or-complain scope-ref name)))
;    ((drawer-get-setter drawer) value)))
