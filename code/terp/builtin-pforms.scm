;;
;; Built-in procs/forms
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
;; TODO (low priority): use vectors instead of lists?
;;   Possibly add extra list argument for varargs?
;;
;;     (define now.var scope node-vector node-list)
;;
;;   There will be a check that node-list is null
;;   for non-vararg functions/forms?
;; TODO: add checks for the correct number of arguments?
;;

;;
;; TODO: provide a second set of PFORMS ('later' pforms?) that
;; output a target language instead of executing
;;
;; (for instance, C++)
;;
;; Keep in mind that the back-end will have to 'manipulate'
;; identifiers: an identifier which is not a reserved word
;; in IGEL might be in the target language, and the adapter
;; has to take care of that.
;;
;; Possibility: have an 'ID ADAPTER LAYER' which provides an
;; Igel identifier and a target-language-identifier comes out.
;; For a C target, for instance, 'default' might be automatically
;; transformed into 'gen0_default'.
;;
;; Provide a way to explicitly map identifiers: for instance, a
;; developer auto-translating an existing Igel library to C can
;; ensure that the function named 'default' in Igel will always be
;; mapped to, say, 'get_default()' in C.
;;

(define (now.var scope nodes)
  ;; Syntax

  ; delegate to semantics

  ;; Semantics
  (bind-list (drawer-name type value) (parse-declaration scope (cdr nodes))
     (now-add-drawer scope drawer-name type value)
    none-singleton))
;;
;; Alternate experimental implementation (requires support
;; function "process-nodelist" in form-support.scm)
;;
; (define (now.var scope nodes)
;   (process-nodelist (cdr nodes)
;                     (lambda l (apply now-add-drawer (cons scope l)))
;                     (list
;                       (lambda (n) (ast-node->drawer-name scope n))
;                       (lambda (n) (ast-node-eval scope n))
;                       (lambda (n) (ast-node-eval scope n))))
;   none-singleton)


(define (now.const scope nodes)
  ;; Syntax

  ; delegate to semantics

  ;; Semantics
  (bind-list (drawer-name type value) (parse-declaration scope (cdr nodes))
    (now-add-const scope drawer-name type value)
    none-singleton))

(define (now.rawattr.__call__ scope nodes)
  (resolve-chain-to-rval resolve-rawattr-chain scope (cdr nodes)))

(define (now.rawattr.__set__ scope nodes)
  (let ((args (cdr nodes)))
    ;; Syntax
    (check-arg-count 2 args "now.rawattr.__set__")

    ;; Semantics
    ;
    ; For now we use bind-list as a shortcut, but we will eventually
    ; need to raise a "now.Exception" in case we have the wrong number
    ; of arguments.
    ;
    ; Should functions take VECTORS of arguments, instead of lists?
    ; Variable list arguments are special designated vector slots. 
    ; This would make calculating size and accessing specific items
    ; faster...
    ;
    ; Maybe we should also do away with passing argv[0]. We will
    ; only pass arguments for now, maybe provide arg0 later if
    ; really needed (in that case, think of plan B)
    ;
    (bind-list (attr-chain-node value-node) args
      (set-using-attr-chain scope
                            (ast-node-value-from-kind attr-chain-node 'list)
                            (ast-node-eval scope value-node)))))

(define (now.dot.__call__ scope nodes)
  (resolve-chain-to-rval resolve-dot-chain scope (cdr nodes)))

;; Placeholder:
(define (now.dot.__set__ scope nodes)
  ;; Syntax check: currently built into semantics

  ;; Semantics
  (let ((tgt-list   (ast-node-value-from-kind (list-ref nodes 1) 'list))
        (value-node                           (list-ref nodes 2)))
    (if (null? tgt-list)
      (eval-error "Cannot pass empty target list to dot.__set__!")
      (let ((head-obj (ast-node-eval scope (car tgt-list)))
            (key-nodes (cdr tgt-list)))
      (resolve-dot-set-chain scope head-obj key-nodes value-node)))))


(define (make-now.delegator-greet kind)
  (lambda (scope nodes)
    (string-append
      "<Hello! Delegator ("
      kind
      ") test here, with arguments ("
      (join (map ast-node->string nodes))
      ")>")))

(define (now.write scope nodes)
  ;; Syntax
  ; -- no check --

  ;; Semantics
  (for-each
    (lambda (node)
      (display (now-val->string (ast-node-eval scope node))))
    (cdr nodes))
  none-singleton)

(define (now.print scope nodes)
  ;; Syntax
  ; -- no check --

  ;; Semantics
  (now.write scope nodes)
  (newline)
  none-singleton)

(define (now.set scope nodes)
    ;;
    ;; Current question...
    ;; WHO EVALUATES THE TARGET?
    ;;
    ;; It should logically be SET before delegating.
    ;; However, delegated __set__ would have a mixed syntax/semantics 
    ;; list.
    ;;
    ;; Turns out it is actually necessary.
    ;;
    ;; See also: commentsi in file ' set-support.scm '
    ;;


  ;; Syntax
  (check-arg-count 2 (cdr nodes) "now.set")

  ;; Semantics
  (let ((target-node (list-ref nodes 1))  ;; TODO: improve logic here, split 'id' and 'list', delegate?
        (value-node  (list-ref nodes 2))) ;; Current concept: delegate evaluation

  ;; Should we return none-singleton ? Delegate?
  ;; Probably delegate?
  (case (ast-node-kind target-node)
    ((id) (set-id-string-to-value scope (ast-node-value target-node) (ast-node-eval scope value-node)))
    ((list) (set-using-invocation scope (ast-node-value target-node) value-node))
    (else => (lambda (kind)
               (error (string-append "Unhandled node kind ('"
                                     (symbol->string kind)
                                     "') as target for now.set")))))))


(define (now.while scope nodes)
  ;; Syntax
  (check-arg-count 2 (cdr nodes) "now.while")

  ;; Semantics

  ;
  ; At the moment, 'while' returns the result
  ; of evaluating its body for the last time
  ; (assuming no control-flow-altering
  ; statements like 'return' were executed)
  ; If no evaluations took place,
  ; none-singleton is returned. Consider: should
  ; it be changed to always return none-singleton ?
  ; (that was the old behaviour)
  ; Should its outcome be 'unspecified'
  ; to allow mapping to different programming
  ; languages?
  ; Or, for mapping purposes, is it better
  ; to leave it like it is now? 
  ;
  (let ((condition-node (list-ref nodes 1))
        (block-node     (list-ref nodes 2)))
    (let loop ((result none-singleton))
      (if (bool-value-true? (ast-node-eval scope condition-node))
        (let ((iteration-outcome (now-eval-block-new-scope scope block-node)))
          (cond
            ((pair? iteration-outcome)
             (case (car iteration-outcome)
               ((return) iteration-outcome) ; pass through unchanged.
               (else (error "Unhnalded iteration-outcome pair"))))
            (else
             (loop iteration-outcome))))
        result))))

;;
;; TODO: perform one syntax check at beginning? Right now, a syntax error
;; (else -> xlse) late in the chain isn't even caught if the else
;; case is never reached...
;;
;; TODO: generic evaluation, not block-evaluation specifically?
;;
;; set x [if [eq y 1] 2 else 3]
;;
;; might be a bit tricky to determine type...
;; Allow to specify type explicitly?
(define (now.if scope nodes)

  (define (if-chain nodes)
    (if (or (null? nodes)
            (null? (cdr nodes)))
      (error "Wrong number of arguments to 'if'")
      (if (bool-value-true? (ast-node-eval scope (car nodes)))
        (now-eval-block-new-scope scope (cadr nodes))
        (else-chain (cddr nodes)))))

  (define (else-chain nodes)
    (cond
      ((null? nodes) none-singleton)
      ((null? (cdr nodes)) ; default unmarked 'else'
       (now-eval-block-new-scope scope (car nodes)))
      (else
        (let ((seen-id (ast-node-value-from-kind (car nodes) 'id)))
          (cond
            ((string=? seen-id "elif") (if-chain (cdr nodes)))
            ((string=? seen-id "else")
             (assert (null? (cddr nodes))) ; Actually a syntax error
             (else-chain (cdr nodes)))
            (else (error (string-append ; Actually a syntax error
                           "Expecting 'else' or 'elsif', got: '"
                           seen-id
                           "'"))))))))
  ;;
  ;; Main body of now.if
  ;;
  (if-chain (cdr nodes)))

(define (now.return scope nodes)
  ;; Syntax
  (check-arg-count 1 (cdr nodes) "now.return")

  ;; Semantics
  (let ((return-value (ast-node-eval scope (list-ref nodes 1))))
    `(return ,return-value)))

;;
;; make-now-proc-callable :
;;
;;    Support function used to turn a signature/body pair
;;    provided by the user (for instance, using 'now.proc')
;;    into a (lambda (scope nodes) ...) that can be invoked
;;    by the evaluator when the syntactic signature of
;;    the defined procedure is encountered.
;;
;; TODO: using a dirty trick now, we skip empty argument entries
;; Formalise definition of "{}" , then adjust the logic
;; to have an error on empty argument entries
;; (but not having any entry is OK -- so, for instance:
;;   { Int a ; ; ; ; } -> NOT ok
;;   {} -> ok
;;
;; ...or should we just always accept and skip
;; empty entries?
;;

;;
;; TODO: check signature syntax at definition time?
;; Makes sense for proc, but not for form...
;;
;; TODO: improve handling of 'undefined for type.
;; Right now we ignore the type, but in the future
;; we might want something that is not a scheme
;; symbol? Maybe an Igel-level symbol? Or an
;; Igel-level singleton (like None in Python)
;;
(define (make-now-proc-callable signature body)

  (define expected-arg-count (length signature)) 

  ;; result of main define (make-now-proc-callable):
  (lambda (scope nodes)
    (define (make-callee-scope)
      (let ((sub-scope (make-sub-scope scope)))
        ;;
        ;; scope-loop: Dynamically creates a local variable for each argument
        ;;             in the function's execution context upon invocation
        ;;             of the function.
        ;;
        (let scope-loop ((arg-signatures-left signature)
                         (nodes-left (cdr nodes))) ; actual parameters of call
          (cond
            ;; TODO: const modifier
            ((pair? arg-signatures-left)
              (let ((current-signature (car arg-signatures-left)))
                (bind-list (drawer-name type default-value) (parse-declaration scope current-signature)
                           ; (display name) (newline)
                  ; (assert (not (eq? type 'undefined)) (error "INTERNAL ERROR: 'undefined type not handled properly. Fix interpreter."))
                  ;; TODO: default values
                  (now-add-drawer sub-scope drawer-name type (ast-node-eval scope (car nodes-left))) ;; TODO: detection and exception if too few arguments
                  (scope-loop (cdr arg-signatures-left) (cdr nodes-left)))))
            ((null? arg-signatures-left)
             (assert (null? nodes-left)) ;; TODO: better compile-time error reporting
             sub-scope)
            (else ;; TODO: gotta get rid of this 'write' (and other 'writes' before errors) ; -- use any->string ?
              (write arg-signatures-left)
              (eval-error "Unknown type for 'arg-signatures-left'"))))))
    ;;
    ;; Main body of lambda:
    ;;

    ;; Syntax -- simple for now (varargs later)
    (check-arg-count expected-arg-count (cdr nodes))

    ;; Semantics
    (let ((outcome (now-eval-block-as-statements (make-callee-scope) body)))
      (cond
        ((pair? outcome)
         (case (car outcome)
           ((return) (cadr outcome)) ;; We have this because 'return'
                                     ;; has to propagate all the way up
                                     ;; to this level.
                                     ;; TODO: find a cleaner approach?
           (else => (lambda (outcome-pair-kind)
                      (string-append "Unknown outcome-pair kind: "
                                     (symbol->string outcome-pair-kind))))))
        (else
          outcome)))))

(define (now.proc scope nodes)

  (define (signature-from-node arg-signature-node)
    ;;
    ;; We might need to clean up empty signatures, due to the way
    ;; {} is implemented.
    ;;
    ;; Should we implement a "remove-null-items-from-block" procedure?
    ;;
    ;; ACTUALLY, TODO: incorporate into the definition of {} that empty
    ;; lists as elements are to be dropped. If one wants empty lists,
    ;; must use an alternate form, for instance {; ;; ;}, or
    ;; {allow-empty: ;; :allow-empty}, or [list [list] [list]].
    ;;
    (assert (ast-node-kind-match? arg-signature-node 'block))
    (let filter-loop ((arg-nodes (ast-node-value arg-signature-node))
                      (result-r '()))
      (if (null? arg-nodes)
        (reverse result-r)
        (let* ((current-node (car arg-nodes))
               (signature-as-list (ast-node-value current-node)))
          (if (null? signature-as-list)
            (filter-loop (cdr arg-nodes) result-r) ; skip current empty entry
            (filter-loop (cdr arg-nodes) (cons signature-as-list result-r)))))))
  ;;
  ;; Main body of " now.proc "
  ;;

  (let ((arg-count (length (cdr nodes))))

    ;; Syntax -- simple for now (varargs later)
    ;; we also cheat a bit on syntax matching.
    ;; For the final version, we will need to re-use
    ;; the parse-signature signature, which will
    ;; understand functions as types
    ;; {...} : Type -> function that takes {...} as arguments and returns type?
    ;; No. That should be [Proc {...} : Type]  , where : is optional.
    ;; We will, however, have to provide a factored parsing function that
    ;; will be shared by the Proc constructor and the proc definition.
    ;;
    ; Later: macro. 
    ;;
    ;; # default: no splicing operator. In order to splice, a splicing operator must be
    ;;            provided explicitly.
    ;;
    ;; template proc -splice "splice" { _name_ ; _signature_ ; { : ;  : Type } ? : _maybe_type_ ; _body_ } [
    ;;    set _name_ [Proc _signature_ [splice: _maybe_type_] _body_] # template interpolation
    ;; ]
    ;;

    (if (or (< arg-count 3)
            (> arg-count 5))
      (error "Wrong number of arguments to now.proc")) ;; TODO: exception
    (if (= arg-count 5)
      (if (not (ast-node-kind-match? (list-ref nodes 3) 'colon))
        (error "Expecting colon in proc definition"))) ;; TODO: exception
    ;; (check-arg-count 3 (cdr nodes) "now.proc")

    ;; Semantics
    (let ((lval (ast-node->drawer-name scope (list-ref nodes 1)))
          (arg-signature-node (list-ref nodes 2))
          ; (maybe-return-type-node   (list-ref nodes (- arg-count 1))) 
          (body               (list-ref nodes arg-count)))
      (let ((signature (signature-from-node arg-signature-node)))
        (now-add-const scope
                       lval
                       "Proc" ;; TODO: this is a placeholder, change!
                       (make-now-proc-callable signature body))
        none-singleton)))) ;; TODO: Should it return none-singleton? The procedure?

;; TODO: rename to 'syntax-quote'?
(define (now.quote-node scope nodes)
  ;; Syntax -- simple for now (varargs later)
  (check-arg-count 1 (cdr nodes) "now.quote-node")

  ;; Semantics
  (list-ref nodes 1))

(define (now.idty scope nodes) ;; Identity function, for testing
  ;; Syntax -- simple for now (varargs later)
  (check-arg-count 1 (cdr nodes) "now.idty")

  ;; Semantics
  (ast-node-eval scope (list-ref nodes 1)))
