;;
;; Dot notation support functions
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
;; Seems like there is a bit of confusion between
;; OBJECTS, SCOPES and NAMESPACES at the moment.
;; Clarify.
;;

;;
;; To consider: accessors (set/get) could be
;; stored within /type/, since all objects of
;; the same type are supposed to share the
;; same logic? It sounds like one of those
;; 'virtual vs. non-virtual methods' scenarios...
;; If we give 'type' precedence, can the
;; implementation of virtual methods
;; become tricky?
;;

;; TODO: should DOT transverse SCOPES or OBJECTS?
;; Do we need DOT for SCOPES?
;; What aout NAMESPACES?
;; Distinguish [dot ...] from [dot -raw ...] ?
;; No, use [rawattr ...] instead of [dot -raw]
;;

;;
;; A note about function calls in chains:
;;
;; How to translate a.b.f().c.d from Python-style notation
;; to square-bracket notation:
;;
;; WRONG: a.b.[f].c
;; RIGHT [a.b.f].c.d
;;
;; * Only leftmost item (starting scope) is object
;; * Other items must be IDs
;;

(define (ast-node->drawer-name scope node)
  ;; ast-node->drawer-name resolves the AST node into
  ;; a potential variable name, which might or might not
  ;; exist, within a scope.
  ;;
  ;; It is used by the likes of 'var' and 'const',
  ;; which need to enter a new variable into a
  ;; scope: they will create new entry if the name
  ;; has no entry associated with it, and report
  ;; an error if the entry already exists.
  ;;
  ;; A non-delegated 'set', conversely, will
  ;; /resolve/ the drawer within the scope, so
  ;; the variable must exist.
  ;;
  (case (ast-node-kind node)
    ((id) (ast-node-value node))
    (else
      (error-and-exit "Unhandled syntax for new namespace entry"))))

(define (rawattr-binary-drawer now-obj key-node)
  (get-now-object-drawer-member-or-complain now-obj (ast-node-value key-node)))
;;
;; Resolve right-hand side (rval-like) of a
;; binary rawattr expression within a chain.
;;
(define (rawattr-binary now-obj key-node)
  (get-drawer-contents (rawattr-binary-drawer now-obj key-node)))

(define (rawattr-binary-set now-obj key-node value)
  (set-drawer-contents (rawattr-binary-drawer now-obj key-node) value))

;;
;; Support for now.rawattr :
;;
(define (resolve-rawattr-chain _unused-scope_ head key-nodes) ;; To conform to API, scope will be passed.
  (let ((rest-of-keys (cdr key-nodes)))
    (if (null? rest-of-keys) (rawattr-binary head (car key-nodes))
        (resolve-key-chain-rval-helper (rawattr-binary head (car key-nodes)) rest-of-keys))))

;;
;; TODO: should we use 'rawattr' as a default... or should we ALWAYS delegate
;; to __dot__, which, by default, coincides with
;; 'rawattr and, if there's arguments left, delegate to next __dot__'?
;;
(define (resolve-dot-chain scope head key-nodes)
  (let resolve-dot-chain-helper ((head head)
                                 (key-nodes key-nodes))
    (if (now-object-has-member? head "__dot__") ;; Note: of course we get it with RAWATTR, or it'd be a chicken-and-egg issue.
      (let ((delegated-dot (resolve-as-scheme-callable (get-now-object-member head "__dot__"))))
        (delegated-dot scope (cons (make-ast-node 'id "__dot__") key-nodes)))
      (let ((rest-of-keys (cdr key-nodes)))
        (if (null? rest-of-keys) (rawattr-binary head (car key-nodes))
            (resolve-dot-chain-helper (rawattr-binary head (car key-nodes)) rest-of-keys)))))) ;; <<--- TODO: this is untested (and there is a typo!)
                                                                                               ;; prepare n-deep chain (with n > 2) to test it.

(define (resolve-chain-to-rval inner-chain-resolver scope nodes)
  (let ((head-drawer (resolve-id-to-drawer-or-complain
                       scope
                       (ast-node-value-from-kind (car nodes) 'id)))
        (rest (cdr nodes)))
    (inner-chain-resolver scope ((drawer-get-getter head-drawer)) rest))) ;; Notice double parenthesis: double call (get getter, call getter)

;;
;; Current concept:
;;
;; We will not have objects representing pointers or references
;; in the bare interpreter.
;; Reading and writing of variables and data structures will be
;; done through methods. If a language needs to represent pointers
;; or references, it can do it at the language definition level.
;; The core infrastructure avoids relying on them.
;;
;; As far as 'lvalues' are concerned, one might argue that
;; a 'drawer' represents an lval. Might actually rename 'drawer'
;; to 'lval' at some point in the future, and 'value' to 'rval'.
;;
;; 'rawattr' never delegates (only uses built-in resolution).
;; 'dot' will try to delegate at every step.
;; TODO: see how Python does things with __getattr__
;;
;; NOTE: the chain behaviour of rawattr is not
;; adequately tested.
;;
(define (resolve-attr-chain-to-drawer head key-nodes)
  (let ((rest-of-keys (cdr key-nodes)))
    (if (null? rest-of-keys) (rawattr-binary-drawer head (car key-nodes))
        (resolve-attr-chain-to-drawer (rawattr-binary head (car key-nodes)) rest-of-keys)))) ; NOT rawattr-binary-drawer

(define (resolve-attr-chain-to-drawer-from-scope scope nodes)
  ; first one is resolved differently: we need to /traverse/ scopes upwards in this case.
  ; TODO: could the first one be an expression? Not handling this at the moment.
  (let ((head-drawer (resolve-id-to-drawer-or-complain
                        scope
                        (ast-node-value-from-kind (car nodes) 'id)))
        (rest (cdr nodes)))
    (if (null? rest)
      head-drawer
      (resolve-attr-chain-to-drawer (get-drawer-contents head-drawer) rest))))

;; Legacy working version which does not support delegation,
;; so can be used for rawattr only
(define (set-using-attr-chain scope attr-chain value)
  (let* ((drawer (resolve-attr-chain-to-drawer-from-scope scope attr-chain))
         (setter (drawer-get-setter drawer)))
    (setter value)
    none-singleton))

;;
;; To support dot.__set__
;;
(define (resolve-dot-set-chain scope head key-nodes value-as-syntax-node)
  (let resolve-dot-set-chain-helper ((head head)
                                     (key-nodes key-nodes))
    ;;
    ;; Consider the following '__set__' on intermediate objects.
    ;; How does it relate, conceptually, to the '__set__' on list heads ( [dot.__set__ x ] -> [ x.__set__ ] )?
    ;; Should it have a different name?
    ;;
    ;; What is the 'head' they pass us? The first object ('x')? The accessor ('dot')?
    ;;
    (if (now-object-has-member? head "__dot__") ;; Note: of course we get it with RAWATTR, or it'd be a chicken-and-egg issue.
      (let ((delegated-dotset (get-callable-for-now.set (get-now-object-member head "__dot__"))))
        (delegated-dotset scope (list
                                  (make-ast-node 'id "__set__")
                                  (make-ast-node 'list key-nodes)
                                  value-as-syntax-node)))
      ;; Base step, non-delegated behaviour
      (if (null? key-nodes)
        (error "No key nodes provided!") ;; TODO: improve
        (let ((rest-of-keys (cdr key-nodes)))
          (cond 
            ((null? rest-of-keys) (rawattr-binary-set head (car key-nodes) (ast-node-eval scope value-as-syntax-node))
                                  none-singleton) ;; <-- delegated result
            (else (resolve-dot-set-chain-helper (rawattr-binary head (car key-nodes))
                                                rest-of-keys))))))))

;; Choose which version to use
; (define set-using-dot-chain set-using-attr-chain)
