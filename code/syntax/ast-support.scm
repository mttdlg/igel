;;
;; Abstract Syntax Tree support
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

(define (list->ast-node l)
  ;; takes:   `(,kind ,value)
  ;; returns: <opaque AST node struct>
  ;;
  ;; TODO: remove, replace with appropraite
  ;; uses of make-ast-node 
  ;;
  (assert (= 2 (length l)))
  (apply make-ast-node l))
  ; (make-ast-node (car l) (cadr l)))

(define (terminator->ast-node token)
  ;;
  ;; Temporary kind used only internally.
  ;; TODO: replace with a different kind?
  ;;
  (make-ast-node 'internal-token token))

(define (token->ast-node token)
  (make-ast-node (token-get-kind token) (token-get-value token)))

(define (ast-node-kind-match? node kind)
  (eq? (ast-node-kind node) kind))

(define (ast-node-value-from-kind node expected-kind)
  (if (ast-node-kind-match? node expected-kind)
    (ast-node-value node)
    (error (string-append "INTERNAL ERROR: attempted to"
                          " extract a value of type '"
                          (symbol->string expected-kind)
                          "' from a node of type '"
                          (symbol->string (ast-node-kind node))
                          "'"))))

;;
;; Syntax match support
;;

(define (ast-node-kind-constant? node)
  (member? (ast-node-kind node) lexer-constant-kinds))

;;
;; Used for tests/debugging only? Double-check:
;;
(define (ast-node->string node)

  (define (ast-node-value->string v)
    (cond
      ((ast-node? v)
       (ast-node->string v))
      ((list? v) (string-append "(" (join (map ast-node-value->string v)) ")"))
      (else
        (any->string v))))

  (string-append
    "{"  (symbol->string (ast-node-kind node))
    ": " (ast-node-value->string (ast-node-value node))
    "}"))
