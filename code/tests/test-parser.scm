
;;
;; Test for the parser
;;
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
;; Old text:
;;  Starting with empty list as context,
;;  replace with proper context later,
;;  when the parser actually honours the
;;  context instead of just ignoring it
;;
;; New text:
;;  Parser now does not deal with semantics.
;;  It just builds the AST.
;;
; (parse-loop (make-context (get-idt-root) ) token-stream)

(define (indent-node node string-prefix)
  (display string-prefix)
  ; (write node)
  ; (newline)
  (cond
    ((not (ast-node? node))
     (display "->")
     (write node)
     (display "<-")
     (newline)
     (error "Candidate node is not ast-node!"))

    ((null? node)
     (error "Null node is not allowed"))

    ;;
    ;; Following does not work, fix:
    ;;
    ((member? (ast-node-kind node) '(list block))
     (let ((list-of-nodes (ast-node-value node)))
       (cond
         ((and (eq? (ast-node-kind node) 'list)
               (null? list-of-nodes))
          (display "[list]")
          (newline))
         (else
           (display "[")
           (display (ast-node-kind node))
           (newline)
           (indent-listlike (ast-node-value node) (string-append string-prefix "  "))
           (display string-prefix)
           (display "]")
           (newline)))))
    (else
      ; (display "else -> ")
      (display (ast-node->string node))
      (newline))))

(define (indent-listlike l string-prefix)
  (map (lambda (n) (indent-node n string-prefix)) l))


; (define p (open-input-file "infile.txt"))
; ...
; (close-input-port p)

(define (test-parser-with-file-name file-name)
  (let* ((char-instream (make-instream-from-file file-name))
         (token-stream (make-token-stream-from-char-instream char-instream)))
         ; (ast (parse-unit token-stream)

    ; (write ast)
    ; (newline)
    ; (indent-node ast "")))))

    (let test-a-line ()
      (let* ((next-token (tstream-peek-token token-stream))
             (token-kind (token-get-kind next-token)))
        (cond
          ((eq? token-kind 'eof) '()) ; End
          (else
            (indent-node (parse-statement token-stream) "")
            (test-a-line)))))))

(define (do-test-parser list-of-files)
  (for-each test-parser-with-file-name list-of-files))
