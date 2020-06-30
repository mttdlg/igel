;;
;; Syntax types
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

;;;;;;;;;;;;
;;        ;;
;; TOKEN  ;;
;;        ;;
;;;;;;;;;;;;

;;
;; We could implement tagged unions to keep the current
;; architecture, but represent the records as proper
;; IGEL values.
;;
;; Alternately, we might make things slightly less
;; type-safe and provide now.Any.
;;
;; The latter approach might be used as an intermediate
;; step before coding a type-safe version.
;;

(define-record-type <token>
                    (make-token kind value location)
                    token?
                    (kind token-get-kind)
                    (value token-get-value)
                    (location token-get-location))

;;
;; Old code defining the same API but implementing the record
;; as a now-object (before now.Object was overhauled -- so it
;; might no longer work)
;;

; (define (make-token token-kind token-value token-location)
;   (make-raw-now-object-from-pairs 'token `(
;         ("kind"     ,(make-now-drawer-const "TokenKind" token-kind))
;         ("value"    ,(make-now-drawer-const "TokenVal"  token-value))
;         ("location" ,(make-now-drawer-const "Location"  token-location)))))
; 
; (define (is-now-object-token? x)
;   ;; We assume argument is now object, we do not check that.
;   (eq? 'token (get-now-object-kind x)))
; 
; (define (token? x)
;   (and (now-object? x) (is-now-object-token? x)))
; 
; (define (token-get-kind token)
;   (assert (is-now-object-token? token))
;   (get-now-object-member token "kind"))
; 
; (define (token-get-value token)
;   (assert (is-now-object-token? token))
;   (get-now-object-member token "value"))
; 
; (define (token-get-location token)
;   (assert (is-now-object-token? token))
;   (get-now-object-member token "location"))

;;;;;;;;;;;;;;;
;;           ;;
;; AST-NODE  ;;
;;           ;;
;;;;;;;;;;;;;;;

;;
;; TODO: needs to contain the position within the input
;; (file + line + column for files) so error messages
;; can report this information.
;;
;; This should be the 'location' object taken from
;; the first token of the node (for a block, it would
;; be the opening curly brace).
;;
(define-record-type <ast-node>
                    (make-ast-node kind value)
                    ast-node?
                    ;; change the following procedures to have a 'get'?
                    ;; Or change the ones with 'get' not to have it?
                    ;; Anyway, standardize.
                    (kind ast-node-kind)
                    (value ast-node-value))


;
; TODO: instead of implementing everything as a now-object, should we
; Implement AST NODES as records (faster), and wrap them into objects only
; when forms actually need to access them?
;
; var n: Igel.AstNode 
;
; n.kind     -> [dot n kind]     -> builtin: accesses record, produces igel-value  
; n.value    -> [dot n value]    -> builtin: accesses record, produces igel-value
; n.location -> [dot n location] -> builtin: accesses record, produces igel-value


; ;; Classes
; (define now.ASTnode
;   (make-now-class;; List of constant kinds, to be used later
;; (for instance, to determine if an AST node
;; is a constant or an expression)
;
;     "now.ASTnode"
;     default-parents
;     (make-default-meta)
;     (make-hash-table)))
; 
; (define (make-ast-node node-class node-value)
;  (make-raw-now-object-from-pairs  `(
;    ("kind"     ,(make-now-drawer-const "NodeKind" node-kind))
;    ("value"    ,(make-now-drawer-const "NodeVal"  node-value)))))
