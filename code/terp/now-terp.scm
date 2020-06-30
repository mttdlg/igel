;;
;; Now-Interpreter
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
; Takes an Abstract Syntax Tree as input,
; interprets commands
;

;
; TODO: Should '-ti', return a specific
; record data structure (terminator/node)
; instead of 'car/cdr' lists?
;

(define (display-args args) 
  (for-each
    (lambda (n)
      (display "  ->  ")
      (display (ast-node->string n))
      (newline))
    args)
  (newline))

; (define (handle-unknown-form line-node)
;     ;;
;     ;;
;     ;; TODO: Distinguish between "unknown command"
;     ;; to "invalid arguments"
;     ;;
;     ;; TODO: better error information
;     ;; (file, line, etc.)
;     ;;
;     (error (string-append "ERROR: unhandled form: "
;                           (ast-node->string line-node))))

;;
;; TODO: move to a specific 'debug' file, not included
;; in the production version of the interpreter?
;;
(define (dbg-handle-form-args form-args ast-args)
  (display (any->string form-args))
  (newline)
  (display-args ast-args))

(define (eval-nodes-as-statement scope-ref list-of-syntax-nodes)
  (if (null? list-of-syntax-nodes)
    (eval-error "It is illegal to evaluate the empty statement with 'eval-nodes-as-statement'.")
    (let ((form-result (execute-form scope-ref list-of-syntax-nodes)))
      (cond
        ((now.none? form-result) form-result) ;; We expect STATEMENTS to not have a return value.
        ; ((igel-value? form-result) form-result)
        ((pair? form-result)
          (case (car form-result)
            ((return) form-result)
            ((void) (error "INTERNAL ERROR: '(void) is obsolete, use now.none instead!"))
            ;;
            ;; TODO: improve handling of results?
            ;;
            ;; Like ('data <kind> ...)
            ;;      ('data 'void)
            ;;      ('eval <...>) // <- macro expansion`
            ;;
            (else
              (error (string-append "Unknown result kind in pair result for statement" (symbol->string (car form-result)))))))
          ; (display (car form))
          ; (display " ")
          ; (dbg-handle-form-args (cdr form) (cdr list-of-syntax-nodes)))
        ;;
        ;; Next case will be replaced by exceptions:
        ;;
        ; ((not form-result) ;; 'form' returned scheme #f
        ;  (handle-unknown-form (make-ast-node 'list list-of-syntax-nodes)))
        (else
          (write form-result) ;; TODO: improve error reporting.
          (newline)
          (eval-error "ERROR: wrong type of result for statement"))))))

; (define (now-eval-statement-node scope line-node)
;   ;;
;   ;; Test/debugging code, for now.
;   ;;
;   ;; TODO: handle macros.
;   ;; Streamline usage of output values.
;   ;;
;   (let ((list-of-syntax-nodes (ast-node-value-from-kind line-node 'list)))
;     (eval-nodes-as-statement scope list-of-syntax-nodes)))

; (define (now-eval-exp node scope)
;   (error "now-eval-exp: not yet implemented"))

(define (now-eval-block-as-statements scope-ref node)
  ;;
  ;; TODO: structure is complex, simplify.
  ;; Maybe split into sub-functions, if possible?
  ;;
  (cond
    ((ast-node-kind-match? node 'block)
     (let now-terp-do-block-lines ((list-of-line-nodes (ast-node-value node))
                                   (last-result now.none))
       (if (null? list-of-line-nodes)
         last-result
         (let ((current-statement-as-list (ast-node-value-from-kind (car list-of-line-nodes) 'list)))
           (if (null? current-statement-as-list) ;; explicitly skip empty statements.
             (now-terp-do-block-lines (cdr list-of-line-nodes) last-result)

             ;;
             ;; Now that all the special case checking is out of the way,
             ;; we can focus on the core logic.
             ;;

             ;; TODO: handle 'break', 'return', 'continue', etc.
             ;; Should 'continuations' of sorts be stored?
             ;; Maybe it's best to call them 'labels'?
             ;; * RETURN must propagate all the way to 'top'-level (proc-level)
             ;; * CONTINUE, BREAK must be able to propagate at least for one block only,
             ;;   but it would be better if the actual level could be specified.
             ;; Allow to 'name' blocks? Innermost blocks shadow outermost?
             ;; (because 'break/continue' is only allowed to break /out/ of a loop,
             ;; so who writes 'break' also wrote everything up to the innermost loop).
             ;; Must find a way to specify 'all the way' (for 'return'). Maybe reserve
             ;; a label called something like '__returning_block'
             ;;
             ;; Question: should semantics of 'break/continue' be hardcoded (TCL-style) or
             ;; do we want some approach where it is built from something more primitive?
             ;; Will the paradigm force the stack semantics?
             ;;
             ;; Proposal: CORE language will work in a manner compatible with a
             ;; stack-based execution model.
             ;;
             ;; If a later-language-implementer wants a scheme-style call/cc where the
             ;; execution model relies on a continuation tree instead of a stack, it is
             ;; always possible to re-implement the CORE language in terms of continuations,
             ;; with call/cc being a non-standard extension. It is okay if CORE language
             ;; (at least for the first draft) is not powerful enough to express call/cc .
             ;; Maybe call/cc might be optional extension later on?
             ;; Kind of like how atomic memory operations are an optional extension
             ;; for RISC-V?
             ;;
             ;; It would, however, be /very/ nice to have a construct for explicitly
             ;; requesting tail call optimization. Look into that...
             ;;
             (let ((result-of-statement (eval-nodes-as-statement scope-ref current-statement-as-list)))
               (if (pair? result-of-statement)
                 (case (car result-of-statement)
                   ((return) result-of-statement) ;; We might have to propagate, otherwise it will only exit the BLOCK.
                   ((void) (error "'void' not expected")) ;; # What about '(void) ?
                   (else =>
                         (lambda (unknown)
                           (error (string-append "Unknown pair-result from statement:"
                                                 (symbol->string unknown))))))
                 (now-terp-do-block-lines (cdr list-of-line-nodes) result-of-statement))))))))
    (else
      ;; TODO: would need better error reporting if
      ;; this was a general-purpose interpreter, with
      ;; the user being able to feed arbitrary nodes.
      ;; But we are using it only for bootstrapping
      ;; purposes at first, so this should suffice.
      ;; For now.
      (error "INTERNAL ERROR: invalid node in AST: expecting block, got something else."))))

(define (now-eval-block-new-scope scope-ref node)
  (define fresh-scope-ref (make-igel-value type-now.ref-to-now.Scope (make-sub-scope scope-ref)))

  (now-eval-block-as-statements fresh-scope-ref node))
;;
;; Main body
;;

(define (now-terp scope-ref ast)
  (now-eval-block-as-statements scope-ref ast))

;;
;; TODO: merge with 'now-terp'?
;;
(define (parse-and-eval-statements scope-ref stream)
  (let eval-loop ((previous-result now.none))
    (let* ((l (parse-statement-checked-ti stream))
           (terminator (car l))
           (the-form (cdr l)))
      (assert (ast-node-kind-match? the-form 'list))
      (let* ((the-form-node-list (ast-node-value the-form))
             (result (if (null? the-form-node-list)
                       previous-result
                       (eval-nodes-as-statement scope-ref the-form-node-list))))
        (if (token-kind-is? terminator 'eof)
          result
          (eval-loop result))))))


