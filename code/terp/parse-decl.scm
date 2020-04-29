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

;;
;; This file deals with parsing declarations
;; of the type:
;;
;; a : int 5
;;
;; Some interpretation support will be provided.
;;
;; Should a full declaration syntax be allowed in assignments?
;;
;; set <declaration>
;;
;; set a : int = 5
;;
;; Probably not in /set/ itself, but the concept should be allowed.
;; For instance, in loops:
;;
;; for var i : int = 1 to 10
;; for var z = 1 to 10
;;
;; ( you add an 'each' to distinguish it form the case with operators: "for each i in ..." for iterators) 
;;
;; ...hmm.
;;
;; Nah.
;;
;; for i : int from 1 to 10
;; for i in [range 1 to 10] # range, closed
;; for i in [range 0 count 10] # range, open
;; Syntactic sugar? Optional type specification?
;;   for i from 1 to 10
;;   for i from 10 down to 1
;;   for i from 10 downto 1

;;
;; First implementation: allow only one type.
;;
;; Later on, we will experiment with multiple types for the same variable.
;; It will be implied that it is an INTERSECTION of types. Like tags.
;; Incompatibilities will be handled only:
;; 1) if they actually pose a problem.
;; 2) manually by the integrator.
;;
;; var ostrich :bird :runner :danger = [Ostrich];
;;

;;
;; TODO: forbid scheme #t and #f as canonical boolean
;; values within the interpreter. Use separate
;; Igel-values as canonical true/false values.
;; (should 'canonical true' and 'canonical false'
;; be objects? Symbols? Both?)
;; Will everything in igel be an object?
;; A better way to word it: perhaps everything
;; will /have/ an associated now.object
;; (and /be/ an object at /compile/ time)
;; but not everything will /be/ a later.object
;; (and /be/ an object at /run/ time)
;;

; TODO: implement proper logic;
; placeholder for now:
(define (compatible-with-type? value type) #t)
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
;;          Fn run {target} = Bird.Runner
;;    }]

;
; Some support functions:
;
(define (new-value-if-ok type new-value)
  (if (eq? type 'undefined)
    new-value
    (if (compatible-with-type? new-value type)
      new-value
      (error "Value and type are incompatible"))))

(define (checked-type-node node)
  ;; TODO: implement list (evaluation)
  ;; For things like [Bits 3 0]
  (case (ast-node-kind node)
    ((id) node)
    (else => (lambda (kind) (error (string-append
                                  "Unhandled type kind "
                                  (symbol->string kind)))))))

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
;; with now-time exceptions.
;;

;; TODO: replace parsing. Have 'parse-all' and, specifically, 'parse-equal'
;;       So that the order is respected, and if colon follows immediately,
;;       we know the colon goes to qualify the value, not the declaration.
;;
(define (parse-declaration-nodes scope signature)
  (define (parse-declaration-value nodes-left type)
    (cond
      ((null? nodes-left) 'undefined)
      ((null? (cdr nodes-left))
       (car nodes-left))
      (else
        (error-and-exit "Parse-value expects one node only!"))))

  ;;
  ;; Main body of "parse-declaration":
  ;;

  ;;
  ;; TODO: since 'value' always comes last, and is always tail-delegated, it seems,
  ;; logic can be simplified and 'value' parameter can be removed from 'parse-rest':
  ;;

  (let ((node-name (car signature)))
    (let parse-rest ((nodes-left (cdr signature))
                     (node-type  'undefined))
      (cond
        ((null? nodes-left)
         (list node-name node-type 'undefined))
        ((null? (cdr nodes-left))
         (list node-name node-type (parse-declaration-value nodes-left node-type)))
        (else
          (let ((next-node (car nodes-left)))
            (case (ast-node-kind next-node)
              ((colon) (let ((new-node-type (checked-type-node (cadr nodes-left))))
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
                                                      ;; (i.e., we change earlier check but
                                                      ;; forget to update this line)
                          (error-and-exit "= not followed by value")
                          (list node-name node-type (parse-declaration-value nodes-after-equal
                                                                             node-type))))))
              (else => (lambda (kind)
                         (error-and-exit (string-append "Invalid kind in declaration: "
                                                        (symbol->string kind))))))))))))

        ;;       Currently not handled: { var foo = 3 : Int }
        ;;       We reserve the syntax '3 : Int'  to mean 3 is Int and not, say, Byte or Uint, in the future.
        ;;       In that case, the whole sequence "3 : Int" will be parsed as a value.

(define (parse-declaration scope signature) ; signature is a list of nodes.

  (define (eval-node-if-defined node)
    (if (eq? node 'undefined)
      'undefined
      (ast-node-eval scope node)))

  (bind-list (node-name node-type node-value) 
             (parse-declaration-nodes scope signature)
    (let ((type (eval-node-if-defined node-type))
          (candidate-value (eval-node-if-defined node-value)))
      (list (ast-node->drawer-name scope node-name)
            type
            (new-value-if-ok type candidate-value)))))

; (ast-node-value-from-kind node-name 'id)
