;;
;; Syntax-Types
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

;;;;;;;;;;;;
;;        ;;
;; TOKEN  ;;
;;        ;;
;;;;;;;;;;;;

;;
;; Original record type, for reference:
;;

(define-record-type <token>
                    (make-token kind value location)
                    token?
                    (kind token-get-kind)
                    (value token-get-value)
                    (location token-get-location))

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
; n.kind     -> [dot n kind]     -> builtin, delegate to record method
; n.value    -> [dot n value]    -> builtin, delegate to record method
; n.location -> [dot n location] -> builtin, delegate to record method
;

; (define (make-ast-node node-kind node-value)
;   (make-raw-now-object-from-pairs 'ast-node `(
;         ("kind"     ,(make-now-drawer-const "NodeKind" node-kind))
;         ("value"    ,(make-now-drawer-const "NodeVal"  node-value)))))
; 
; (define (is-now-object-ast-node? x)
;   ;; We do not check that 'x' is a now-object.
;   ;; The caller is responsible for ensuring that.
;   (eq? 'ast-node (get-now-object-kind x)))
; 
; (define (ast-node? x)
;   (and (now-object? x) (is-now-object-ast-node? x)))
; 
; (define (ast-node-kind node)
;   (assert (is-now-object-ast-node? node))
;   (get-now-object-member node "kind"))
; 
; (define (ast-node-value node)
;   (assert (is-now-object-ast-node? node))
;   (get-now-object-member node "value"))
