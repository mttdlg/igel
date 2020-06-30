;;
;; Parsing of declarations
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

;
; This file deals with parsing declarations
; expressed in one of these formats:
;
; a
; a 5
; a = 5
; a : int
; a : int 5
; a : int = 5
;
; Question: Should a full declaration syntax (type included) be allowed in assignments?
;
; set <declaration-like-syntax>
;
; set a : int = 5
;
; Probably not in /set/ itself, but the concept should be allowed elsewhere.
; For instance, in loops:
;
; for var i : int = 1 to 10
; for var z = 1 to 10
;
; ( you add an 'each' to distinguish it form the case with operators: "for each i in ..." for iterators) 
;
; ...hmm.
;
; Nah.
;
; for i from 1 to 10 { #  type is inferred from extremities of range
; for i : int from 1 to 10 { # type is explicitly given
; for i : int in [range 1 to 10] # range, closed. Explicit typing.
; for i in [range 1 to 10] # range, closed. Type inferred.
; for i in [range 0 count 10] # range, open
; Syntactic sugar? Optional type specification?
;   for i from 1 to 10
;   for i from 10 down to 1
;   for i from 10 downto 1

;
; First implementation: allow only one type.
;
; Later on, we will experiment with multiple types for the same variable.
; It will be implied that it is an INTERSECTION of types. Like tags.
; Incompatibilities will be handled only:
; 1) if they actually pose a problem.
; 2) manually by the integrator.
;
; var ostrich :bird :runner :danger = [Ostrich];
;

;
; ...we need to come up with a concept for
; a 'type combiner' ('integrator' in
; digital design terms -- but 'integrator'
; could be a confusing name, e.g., could be
; clashing with the concept of 'antiderivative'
; in some contexts.
;
; Come up with better name?
;

;;
;; Current concept -> var ostrich : [DefaultTypeIntegrator Bird Runner]
;; Current concept -> var ostrich : [TrivialTypeIntegrator Bird Runner]
;; Current concept -> var ostrich : [ManualTypeIntegrator  Bird Runner {
;;          fn run {target} = Bird.Runner
;;    }]

;
; Some support functions:
;
(define (new-value-if-ok type new-value)
  (if (eq? type 'undefined)
    new-value
    (if (type-accepts-value? type new-value)
      new-value
      (error "Type and value are incompatible in declaration")))) ;; TODO: now.Exception

; (define (checked-type-node scope-ref node)
;   ;; TODO: implement list (evaluation)
;   ;; For things like [Bits 3 0]
;   (let ((result (ast-node-eval scope-ref node)))
;     (if (type-accepts-value? now.type result)
;         result
;         (error ("Expecting type, got something else instead."))))) ; TODO: now.Exception

;
; Main declaration-parsing function
;
; TODO: handle modifiers (const)
; a: const MyStruct -> NAME : MODIFIER TYPE -> symbol 'a' is constant
; b: [MyStruct -const] -> NAME : TYPE -> symbol 'a' is variable. '-const' is now a parameter to the type constructor.
; a: const [MyStruct -const] -> Both constant, of course.
;
; If it's after the colon and at the same nesting level: it refers to variable (same level)
; Part of type constructor: it affects type, of course.
;

;;
;; TODO: replace 'error-and-exit' until the end of the file
;; with now.Exception's.
;;

;; TODO: replace parsing. Have 'parse-all' and, specifically, 'parse-equal'
;;       So that the order is respected, and if colon follows immediately,
;;       we know the colon goes to qualify the value, not the declaration.
;;
(define (extract-declaration-nodes scope-ref declaration-nodes) ;; Will return nodes, will not evaluate
  (define (parse-declaration-value nodes-left type)
    (cond
      ((null? nodes-left) 'undefined)
      ((null? (cdr nodes-left))
       (car nodes-left))
      (else
        (error-and-exit "parse-declaration-value got more than one node")))) ;; Actually, internal error? Not a now.Exception ?
                                                                             ;; ...should internal errors also be now.Exceptions?
                                                                             ;; To provide workarounds until a new fixed version
                                                                             ;; of the now-interpreter is released...

  ;;
  ;; Main body of "extract-declaration-nodes":
  ;;

  ;;
  ;; TODO: since 'value' always comes last, and is always tail-delegated, it seems,
  ;; the logic can be simplified and the 'value' parameter can be removed from
  ;; 'parse-rest':
  ;;

  (let ((node-name (car declaration-nodes)))
    (let parse-rest ((nodes-left (cdr declaration-nodes))
                     (node-type  'undefined))
      (cond
        ((null? nodes-left)
         (list node-name node-type 'undefined))
        ((null? (cdr nodes-left))
         (list node-name node-type (parse-declaration-value nodes-left node-type)))
        (else
          (let ((next-node (car nodes-left)))
            (case (ast-node-kind next-node)
              ((colon) (let ((new-node-type (cadr nodes-left)))
                         (if (not (eq? node-type 'undefined))
                           (error-and-exit "Multiple type declarations.")
                           (parse-rest (cddr nodes-left)
                                       new-node-type))))
              ((id) (if (not (string=? (ast-node-value next-node) "=")) ; Replace with 'expect'?
                      (error-and-exit (string-append "Invalid ID in declaration : " (ast-node-value next-node)))
                      (let ((nodes-after-equal (cdr nodes-left)))
                        (if (null? nodes-after-equal) ;; This check has already been performed
                                                      ;; earlier, but we leave it around for
                                                      ;; robustness to non-local changes
                                                      ;; (i.e., in case we change the code
                                                      ;; where the earlier check is, but
                                                      ;; forget to update this section of
                                                      ;; the code)
                          (error-and-exit "'=' in declaration not followed by value")
                          (list node-name node-type (parse-declaration-value nodes-after-equal
                                                                             node-type))))))
              (else => (lambda (kind)
                         (error-and-exit (string-append "Invalid AST node kind in declaration: "
                                                        (symbol->string kind))))))))))))

        ;;       Currently not handled: { var foo = 3 : int }
        ;;       We reserve the syntax '3 : int'  to mean 3 is Int and not, say, byte or uint, in the future.
        ;;       In that case, the whole sequence "3 : int" will be parsed as a value.
        ;;       (we will have to see if this is feasible, or if it will create problems)
        ;;       It might be simpler to write something like [int 3] instead.

(define (parse-declaration scope-ref declaration-nodes) ; declaration-nodes is a list of nodes.

  (bind-list (node-name node-type node-value) 
             (extract-declaration-nodes scope-ref declaration-nodes)
    (let ((type (eval-node-if-defined node-type))
          (candidate-value (eval-node-if-defined node-value)))
      (if (not (type-accepts-value? now.type type))
          (error "Invalid type in declaration")) ; TODO: now.Exception
      (list (ast-node->drawer-name scope-ref node-name)
            type
            (new-value-if-ok type candidate-value)))))

; (ast-node-value-from-kind node-name 'id)
