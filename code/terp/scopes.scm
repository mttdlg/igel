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
;; In the future, it might be worth considering
;; whether or not it makes sense to have an "id-table"
;; record-type which is used by both scopes and
;; namespaces.
;;

(define (make-sub-scope scope)
  (make-raw-now-object-from-pairs 'scope `(
    ("id-table" ,(make-hash-table)) ;; TODO: replace with NAMESPACE. Also provide a 'scope-get-namespace ' convenience function!
    ("parent"  ,scope)
    ("on-exit" (fresh-igel-empty-list)) ; For now, empty list. In future, hooks.
                   ; TODO: remember to call the hooks!
                   ; on-exit? on-leave?
                   ; TODO: will have to be an igel-list
    ; ("__dot__" -> "__call__", "__set__")
  )))

(define (get-scope-table scope)
  (get-now-object-raw-member scope "id-table"))

(define (get-scope-parent scope)
  (get-now-object-raw-member scope "parent"))

;;
;; Top scope is pretty much like any other scope,
;; except its 'parent' scope is none-singleton .
;;
(define top-scope (make-sub-scope none-singleton))

;;
;; extract-drawer-or-complain looks only in the
;; scope passed as argument.
;; It will not search parent/ancestor scopes
;; if the key is not found in the scope
;; passed as argument.
;;
;; TODO: split into try-extracting-drawer and
;; extract-drawer-or-complain?
;;
(define (extract-drawer-or-complain scope name)
  (let ((scope-table (get-scope-table scope)))
    (if (hash-table-exists? scope-table name)
      (hash-table-ref scope-table name)
      (scope-error (string-append "Identifier not in scope table: " name)))))

;;
;; Support functions which work with drawers:
;;
;; TODO: change (or add new function(s)) so that
;; it takes ID NODE instead of NAME.
;;
(define (try-resolving-drawer scope name)
  (if (none-singleton? scope)
    #f
    (let ((scope-table (get-scope-table scope)))
      (if (hash-table-exists? scope-table name)
        (hash-table-ref scope-table name)
        (try-resolving-drawer (get-scope-parent scope) name)))))

(define (add-drawer-to-scope scope name drawer)
  ;; TODO: Check for existence first!
  (let ((scope-table (get-scope-table scope)))
    ; (write scope-table)
    ; (newline)
    (if (hash-table-exists? scope-table name)
      (scope-error (string-append "Attempting to add an already"
                                  " existing id to scope: '"
                                  name "'"))
      (hash-table-set! scope-table
                       name
                       drawer))))

;;
;; Creating drawers:
;;

(define (now-add-drawer scope name kind value)
  (add-drawer-to-scope
    scope
    name
    (make-now-drawer-var kind value)))

(define (now-add-const scope name kind value)
  (add-drawer-to-scope
    scope
    name
    (make-now-drawer-const kind value)))

(define (resolve-id-to-drawer-or-complain scope name)
  (let ((maybe-drawer (try-resolving-drawer scope name)))
    (if maybe-drawer
      maybe-drawer
      (scope-error (string-append
                     "Could not resolve identifier: '"
                     name
                     "'")))))

;;
;; Working with data directly:
;;

(define (get-drawer-contents drawer)
  ((drawer-get-getter drawer))) ;; Notice: double parenthesis.

(define (set-drawer-contents drawer value)
  ((drawer-get-setter drawer) value)) ;; Notice: double parenthesis.

(define (now-get-drawer scope name)
  (let ((drawer (resolve-id-to-drawer-or-complain scope name)))
    (get-drawer-contents drawer)))

(define (now-set-drawer-value scope name value)
  (let ((drawer (resolve-id-to-drawer-or-complain scope name)))
    ((drawer-get-setter drawer) value)))
