;;
;; Parser
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
;; Parsing proceeds like this:
;;
;; top-level statements are /always/ parsed into a list of AST nodes according to
;; existing rules, and *then* now-evaluated. Early drafts considered an extensible
;; parser (Forth-style, where the now-language would get a chance to get at the
;; character stream).
;;
;; To parse custom sytax (for instance, assembly) the parser will return a STRING,
;; and then now-language will get a go at procducing AST objects from the string.
;;
;; {:asm:
;;          mov r1, 10
;;      loop:
;;          bz  r1, end
;;          sub r1, 1
;;          jp loop
;;      end:
;;          ret
;; :asm:}
;;
;; The whole block will be parsed as a string, and returned as such.
;; It will be then be possible to write a parser for the string in the
;; now-language. The /character stream/ itself, though, has not been
;; exposed, manipulation is performed at the AST level.
;;

;;
;; TODO: change the simple name 'stream' to 'tstream' or 'token-stream'
;;

;;
;; TODO:
;; add extra info to report runtime errors?
;; (like, 'Runtime error in line ...')
;; Also, code coverage. Code coverage is important.
;;
;; NOTE:
;;  forms may match SYNTACTIC CLASS (id, list, etc.).
;;        they must explicitly evaluate nodes, and
;;        then, maybe, check types.
;;
;;        form Proc while {Expression exp; Block block} {
;;           set cx [compile-exp Bool exp]
;;           set cb [compile-block block]
;;           gen-label expression_label # now-expression
;;           gen-label end_label        # now-expression
;;           asm-out {
;;              label expression_label
;;              emit cx
;;              emit-asm { bnez a0 end_label }
;;              emit cb 
;;              emit-asm { j expression_label }
;;              label $end_label 
;;           }
;;        }
;;


(define (parser-error msg)
  ; TODO: improve, pass token, so we can extract
  ; the token's location.
  (error msg))

;; ID-chain resolution and support routines
;;

;;
;; @private
;; @fun parse-dottable
;;
(define (parse-dottable stream)
  (define (parse-dottable-helper result-r)
    (let* ((tok-first (tstream-peek-token stream))
           (tok-first-kind (token-get-kind tok-first))
           (parsed-base 
             (case tok-first-kind
               ; each case-clause is responsible for leaving the stream aligned
               ; at the point just after the dottable (so that
               ; 'tstream-peek-token' will return 'dot' if there is one)
               ((id) (token->ast-node (tstream-read-token stream))) ; (tstream-read-token stream): the same
                                                           ; value as tok-first, except that
                                                           ; we also advance the stream.
               ((left-square) (parse-square stream))
               (else (parser-error (string-append 
                                     "This syntactic element cannot"
                                     " be part of a dottable's path: "
                                     (symbol->string tok-first-kind))))))
           (new-result-r (cons parsed-base result-r))
           ; tok-maybe-dot needs to be last, so 'parse-base' can work its magic
           ; and align the stream properly before we evaluate the expression for
           ; tok-maybe-dot (one more reason to use 'let*' instead of 'let').
           (tok-maybe-dot (tstream-peek-token stream)))
    (cond 
      ((token-kind-is? tok-maybe-dot 'dot)
       (tstream-read-token stream) ; discard the dot
       (parse-dottable-helper new-result-r)) ; note: it's 'new-result-r', not 'result-r'
      (else (reverse new-result-r)))))       ; same here.

;;
;; Main body
;;
;; parse-dottable-helper pretty much does all the work.
;; The main body takes care of reworking the output of
;; parse-dottable-helper into a standardised form.
;

  (let ((parsed-dottable (parse-dottable-helper '())))
    (assert (pair? parsed-dottable)
            "INTERNAL ERROR: parse-dottable-helper returned a non-pair")
    ;; Should we always go with the "dot" form? Or is this
    ;; 'smart' form okay?
    (cond
      ((null? (cdr parsed-dottable)) (car parsed-dottable))
      (else
        (make-ast-node 'list
                       (cons (make-ast-node 'id "dot") parsed-dottable))))))

(define (line-terminator? token)
  (member? (token-get-kind token) '(eof eol)))

(define (strict-form-terminator? token)
  ;; Tokens which always temrinate a
  ;; form no matter what we are parsing.
  ;;
  ;; In other words, if it /will/ terminate
  ;; a form, no matter what the context,
  ;; the result is #t.
  (let ((token-kind (token-get-kind token)))
    (or (member? token-kind '(right-round right-square right-curly eof))
        (and (eq? token-kind 'eol)
             (not (char-whitespace? (token-get-value token)))))))

(define (loose-form-terminator? token)
  ;; Tokens which either always terminate
  ;; a form, or occasionally terminate
  ;; a form (for instance, newline terminates
  ;; a form while parsing a block, but not
  ;; while parsing a square-brackets
  ;; expression).
  ;;
  ;; In other words: if it /might/ terminate
  ;; a form, the result is #t
  ;;
  ;; The following expression is not optimized for speed,
  ;; but for readability and ease of mantenance
  (or (strict-form-terminator? token)
      (line-terminator? token)))

;;
;; This is used only for reporting an error in parse-square,
;; which is supposed to be refactored away anyway. Make sure
;; you move this procedure there when the time comes.
;;
(define (token-semicolon? token)
  (and (token-kind-is? token 'eol)
       (char=? (token-get-value token) #\;)))

;;
;; parse-item
;;
(define (parse-item stream)
  (let ((token (tstream-peek-token stream)))
    ;; TODO: clean up handling of line terminators
    (if (loose-form-terminator? token)
      (terminator->ast-node (tstream-read-token stream))
      (let ((token-kind (token-get-kind token)))
        (case token-kind
          ((whitespace)   (tstream-read-token stream)         ;; discard
                          (parse-item stream))        ;; repeat attempt
          ((id)           (parse-dottable stream))
          ((left-square)  (parse-dottable stream))
          ((left-curly)   (parse-curly stream))      ;; list (of-lists), and possibly special syntax {:foo: :foo:} in the future.
          ((left-round)   (parse-round stream))      ;; special syntax
          
          ;; Pass anything else as-is
          (else
            (token->ast-node (tstream-read-token stream))))))))

;;
;; TODO: split PARSE-FORM off into:
;; * parse form-id 
;; * parse arguments
;;

;;
;; Will be used for both "parse-square" and
;; for parsing a single line within a block.
;;
;; The difference is that a new line is a
;; line terminator in block mode, but just
;; whitespace in expression (parse-square)
;; mode. We will distinguish the two cases
;; by passing different check-for-terminator
;; predicates to this function.
;;
;; The -ti suffix means that the function provides 'Terminator Information'
;; as part of the result (in some cases, like 'parse-statement', we might
;; have two versions of the same function, one with '-ti' and one without).
;;
(define (parse-form-with-terminator-predicate-ti stream form-terminator?)
  ;;
  ;; Returns a list:
  ;;
  ;; 'car' is symbol containing the terminator
  ;; 'cdr' is the contents (if valid), always represented as an AST node whose kind is 'list
  ;;
  ;; TODO: make it return a special 'parsed-line' struct?
  ;;
  (let parse-form-helper ((accumulated-items-r '()))
    (let ((pitem (parse-item stream))) ; parsed item
      ;; (write pitem) (newline)
      (if (not (ast-node-kind-match? pitem 'internal-token)) ;; TODO: instead of an ast-kind,
                                                             ;; return a different record type.
        (parse-form-helper (cons pitem accumulated-items-r))
        ;; Handle internal token
        (let ((token (ast-node-value pitem)))
          (cond
            ((form-terminator? token) ; THe order of checks is important: if 'eol is a terminator, it must be caught here.
             (cons token (make-ast-node 'list (reverse accumulated-items-r))))
            ((token-kind-is? token 'eol) ; If we got here, 'eol is not considered a terminator.
             (parse-form-helper accumulated-items-r))  ; Discard it and proceed
            (else
               (parser-error (string-append "Unexpected token: "
                                            (symbol->string (car token)))))))))))
;;
;; Line-parsing functions.
;; Part of public interface?
;;
(define (parse-expression-ti stream)
  (parse-form-with-terminator-predicate-ti stream strict-form-terminator?))

(define (parse-statement-ti stream)
  (parse-form-with-terminator-predicate-ti stream loose-form-terminator?))

(define (parse-statement-checked-ti stream)
  (let* ((parsed-form-info (parse-statement-ti stream))
         (terminated-by-token (car parsed-form-info)))
    (if (line-terminator? terminated-by-token)
      parsed-form-info
      (parser-error (string-append "Unexpected end-of-line character '"
                                   (symbol->string (token-get-kind terminated-by-token))
                                   "'.")))))

(define (parse-statement stream)
  (cdr (parse-statement-checked-ti stream)))  ;; cdr -> strip terminator at the beginning
;;
;; parse-block-with-terminator will be used for both "parse-curly" and
;; "parse-unit", by passing it different expected block terminator kinds.
;;
;; If we change the way we interpret units (shall we have a separate loop?)
;; then we might drop the 'expected-block-terminator-kind' argument and just
;; assume it is a closed curly brace.
;; 
;; But if we /do/ have the terminator-kind argument, we pass the 'kind' instead
;; of a predicate that tests for the right kind, so we can print an appropriate
;; error message in case we get the wrong terminator.
;;
(define (parse-block-with-terminator-kind stream expected-block-terminator-kind)
  (let parse-statement-within-block ((accumulated-lines-r '()))
    (let* ((parsed-form-info (parse-statement-ti stream))
           (terminated-by-token (car parsed-form-info))
           (terminated-by-kind (token-get-kind terminated-by-token))
           (contents (cdr parsed-form-info))
           (new-accumulated-lines-r (cons contents accumulated-lines-r)))
      ; (display terminated-by-token) (newline)
      (cond 
        ((eq? terminated-by-kind expected-block-terminator-kind)
         (make-ast-node 'block (reverse new-accumulated-lines-r)))
        ((eq? terminated-by-kind 'eol)
         (parse-statement-within-block new-accumulated-lines-r)) ;; Append and proceed
        (else
          (parser-error (string-append "Within a block expected to end with '"
                                       (symbol->string expected-block-terminator-kind)
                                       "', got an unexpected '"
                                       (symbol->string terminated-by-kind)
                                       "'")))))))

(define (consume-token-kind stream expected-kind)
  (let* ((token (tstream-read-token stream))
         (observed-kind (token-get-kind token)))
    (if (eq? expected-kind observed-kind)
      token
      (error (string-append "INTERNAL ERROR:"
                            " Expecting a " expected-kind
                            " but got a " observed-kind
                            " instead")))))

(define (parse-curly stream)
  ;; For now, only standard curly braces, no special syntax like {:foo: ... :foo:}
  (consume-token-kind stream 'left-curly)
  (parse-block-with-terminator-kind stream 'right-curly))

(define (parse-round stream)
  (consume-token-kind stream 'left-round)
  (parser-error "parse-round has not been implemented yet"))

(define (parse-square stream)
  (consume-token-kind stream 'left-square)
  (let* ((parsed-form-info (parse-expression-ti stream))
         (terminated-by-token (car parsed-form-info)))
         
    (if (token-kind-is? terminated-by-token 'right-square)
        (cdr parsed-form-info)
        ;;
        ;; else: print an error message
        ;; (that's all the rest is, really. An error message)
        ;;
        ;; Should probably make 'terminator->verbose-name' a function
        ;;
        (let* ((terminated-by-kind (token-get-kind terminated-by-token))
               (terminated-by-value (token-get-value terminated-by-token))
               (printable-tby-string
                 (cond
                   ((null? (terminated-by-value)) (symbol->string terminated-by-kind))
                   ((token-semicolon? terminated-by-token) "semicolon")
                   ((eq? terminated-by-kind 'eol) "end of line (is this even supposed to happen?)") ; This should never happen
                   (else (parser-error "Unhandled terminator token while trying to print error message")))))
          (parser-error (string-append "Expecting expression to end with 'right-square', got '"
                                        printable-tby-string "' instead."))))))

;;
;; TODO: we'll probably get rid of the following one?
;;

;;
;; @public
;; @fun parse-unit
;;      Parse a source unit (file at the moment)
;; @arg stream : token-stream
;;      An input token stream 
;; @ret : ast
;;      An abstract syntax tree
;;      (root node is expected to be a list of statements)
;; @side-effects : consumes data from input stream
;;
(define (parse-unit stream)
  (parse-block-with-terminator-kind stream 'eof))

