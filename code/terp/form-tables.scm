;;
;; Form Tables
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

;;
;; A first set of forms, for testing
;; Later on we might replace this with
;; a hash table.
;;
;; This definitely needs to be moved
;; to a different file.
;;

;;
;; TODO: pluggable now/later semantics.
;;

;;
;; TODO: another point to consider, concept-wise.
;;
;; Should the 'add' in [add 3 2] /resolve/ to now.add
;; (which requires thinking a proper type-resolution-system),
;; or should 'add' resolve to later.add, and later.add notices
;; it can optimize everything away and return a constant '5'?
;;
;; Possibility 1: "tag" values that can be 100% resolved 
;; at compile time (known-now) with an appropriate type.
;; Use slot compatibility to see if now.add applies or not.
;;
;; If possibility 1 cannot be implemented for any reason
;; (change in type system?), perhaps it is better to always
;; resolve to now.* by default, and it will determine
;; whether to handle things or pass them on to later.add?
;;
;; Actually... perhaps it is better to think of a fully
;; structured delegation system, possibly type-based?
;;

;;
;; TODO: consider splitting scopes into:
;; 1) PROC scopes
;; 2) FN   scopes
;; 3) VAR  scopes
;; (so... if it was a LISP, we'd have a LISP-3 ?)
;;

(define (extend-scope-with-form-info scope-to-extend type form-info)
  (now-add-const
    scope-to-extend
    (car form-info)
    type
    (cadr form-info)))

;;
;; Helpers for defining
;; built-in function forms
;;
(define (make-now-fn-binary-int fn)
  (lambda (scope nodes)
    ;; Syntax
    (check-arg-count 2 (cdr nodes)) ;; TODO: raise SYNTAX ERROR, not assertion failure

    (let ((a (ast-node-eval scope (list-ref nodes 1)))
          (b (ast-node-eval scope (list-ref nodes 2))))
      ;; TODO: for now we cheat and use '<'.
      ;; Implement better approach in the future!
      (let ((result (fn a b)))
        (assert (exact? result))
        (assert (integer? result))
        result))))

; (define (not= a b) (not (= a b)))
(define (not= . l) (not (apply = l)))

;;
;; Built-in procedure forms
;;
(define (extend-scope-with-builtin-forms-proc scope-to-extend)

  ;;
  ;; Make now-stuff return a list to compile?
  ;; Should their return value be irrelevant?
  ;; Possibly the latter.
  ;;
  ;; What about macros? Maybe now-stuff should
  ;; return either:
  ;; #f if all that was to be done was done
  ;; A list if macro expansion happened, and a new
  ;; command needs to be re-evaluated? Hm.
  ;; This is tricky, because it might be /expressions/
  ;; that are macro-expanded...
  ;;
  ;; Should argument list be a vector instead of a list?
  ;; Or, should we have a 'name-arguments' macro?
  ;; Like, (ast-node-let (foo bar baz) node-list)
  ;;

  (for-each
    (lambda (info)
      (extend-scope-with-form-info scope-to-extend "Proc" info))
    `(
      ("rawattr" ,(make-raw-now-object-from-pairs
                   "Proc"
                   `(("__call__" ,(make-now-drawer-const "Proc" now.rawattr.__call__))
                     ("__set__"  ,(make-now-drawer-const "Proc" now.rawattr.__set__)))))
      ("dot"    ,(make-raw-now-object-from-pairs
                   "Proc"
                   `(("__call__" ,(make-now-drawer-const "Proc" now.dot.__call__))
                     ("__set__"  ,(make-now-drawer-const "Proc" now.dot.__set__)))))
      ("var"    ,now.var    )
      ("const"  ,now.const  )
      ("write"  ,now.write  )
      ("print"  ,now.print  )
      ("set"    ,now.set    )
      ("while"  ,now.while  )
      ("if"     ,now.if     )
      ("return" ,now.return )
      ("proc"   ,now.proc   )
      ; ("if" (exp ( ? then: ) block ( * elsif: cond block ) ( ? else: block)) ,now-if)
      )))


;   (define (now-fn-add-n scope nodes)
;     ;; TODO: quick and dirty implementation,
;     ;; uses built-in "+"! Change this.
;     (let*
;       ((evaluated-nodes (map (lambda (node)
;                                (ast-node-eval scope node))
;                              (cdr nodes)))
;        (sum (apply + evaluated-nodes)))
;       (assert (exact? sum))
;       (assert (integer? sum)) ;; for now.
;       sum))

;;
;; Built-in function forms:
;;
(define (make-now-fn-binary fn)
  (lambda (scope nodes)
    (let ((args (cdr nodes)))
      ;; Syntax
      (check-arg-count 2 args) ;; TODO: raise SYNTAX ERROR, not assertion failure

      ;; Semantics
      (bind-list (a b) args
        ;; TODO: for now we cheat and use '<'.
        ;; Implement better approach in the future!
        (fn (ast-node-eval scope a)
            (ast-node-eval scope b))))))

;;
;; TODO: move to library/generic utils?
;;       or at least to a file named
;;       "primitives.scm"?
;;
(define (pad-list orig-list min-size padding-element)
  (let pad-list-helper ((l orig-list)
                        (size (length orig-list)))
    (if (>= size min-size)
      l
      (pad-list-helper (cons padding-element l) (+ size 1)))))

(define (integer:output-base:min-size->string int-num base min-size)
  (define digit-vector '#( #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                           #\8 #\9 #\a #\b #\c #\d #\e #\f
                           #\g #\h #\i #\j #\k #\l #\m #\n
                           #\o #\p #\q #\r #\s #\t #\u #\v
                           #\w #\x #\y #\z))

  (define (to-base-digits n)
    (assert (<= base (vector-length digit-vector)))
    (let base-loop ((n n)
                   (digits '()))
      (assert (exact? n))
      (cond
        ((> n 0) (base-loop (quotient n base)
                            (cons (remainder n base) digits)))
        ((= n 0) (if (null? digits) '(0) digits))
        ((< n 0) (error "Cannot handle negative numbers (yet)")))))

  (let* ((list-of-digit-chars (map (lambda (digit)
                                     (vector-ref digit-vector digit))
                                   (to-base-digits int-num))))
    (list->string (pad-list list-of-digit-chars min-size #\0))))

(define (now-base-vararg scope fn-base args)
  (case (length args)
    ((0) "")
    ((1) (fn-base (ast-node-eval scope (car args)) 0))
    ((2) (bind-list (num-node min-size-node) args
           (fn-base (ast-node-eval scope num-node)
                    (ast-node-eval scope min-size-node))))
    (else
      error "Wrong number of arguments to base-printing function!"))) ;; Syntax error, not just error -> will need to be a now.exception, so we catch it and print the correct error message.

(define (now.hex scope nodes)
  (define (fn-hex num min-digits)
    (string-append "0x" (integer:output-base:min-size->string num 16 min-digits)))

  ;; Check syntax, dispatch to corresponding semantics
  (now-base-vararg scope fn-hex (cdr nodes)))

(define (now.bin scope nodes)
  (define (fn-bin num min-digits)
    (string-append "0b" (integer:output-base:min-size->string num 2 min-digits)))
  (now-base-vararg scope fn-bin (cdr nodes)))

(define (now.cat scope nodes)
  (apply string-append
         (map
           (lambda (node)
             (now-val->string (ast-node-eval scope node)))
           (cdr nodes))))

(define now.knum (make-raw-now-object-from-pairs 'namespace
  `(
      ("one"   ,(make-now-drawer-const "Int" 1))
      ("two"   ,(make-now-drawer-const "Int" 2))
      ("three" ,(make-now-drawer-const "Int" 3))
      ("x"     ,(make-now-drawer-var   "Int" 4))
  )))

(define now.delegator (make-raw-now-object-from-pairs 'namespace
  `(
      ("__dot__" ,(make-now-drawer-const
                    "Proc"
                    (make-raw-now-object-from-pairs
                      "Proc"
                      `(("__call__" ,(make-now-drawer-const "Proc" (make-now.delegator-greet "getter")))
                        ("__set__"  ,(make-now-drawer-const "Proc" (make-now.delegator-greet "setter")))))))
      ("one"   ,(make-now-drawer-const "String" "delegator rawattr one"))
      ("two"   ,(make-now-drawer-const "String" "delegator rawattr two"))
      ("three" ,(make-now-drawer-const "String" "delegator rawattr three"))
      ("x"     ,(make-now-drawer-var   "String" "delegator rawattr x"))
  )))

;;
;; TODO: turn into hash-function instead of association list.
;;
(define (extend-scope-with-builtin-forms-fn scope-to-extend)
  (for-each
    (lambda (info)
      (extend-scope-with-form-info scope-to-extend "Fn" info))
    `(
      ;; Comparison operators
      ("eq"   ,(make-now-fn-binary =    ))
      ("ne"   ,(make-now-fn-binary not= ))
      ("lt"   ,(make-now-fn-binary <    ))
      ("le"   ,(make-now-fn-binary <=   ))
      ("gt"   ,(make-now-fn-binary >    ))
      ("ge"   ,(make-now-fn-binary >=   ))

      ;; Mathematical operators
      ("add"  ,(make-now-fn-binary-int + ))
      ("sub"  ,(make-now-fn-binary-int - ))
      ("mod"  ,(make-now-fn-binary-int modulo))
      ("idiv" ,(make-now-fn-binary-int quotient))

      ;; string construction
      ("hex"  ,now.hex)
      ("bin"  ,now.bin)
      ("cat"  ,now.cat)
      
      ;; Special
      ;;
      ("idty"       ,now.idty)       ;; "IDentiTY": Test function: just evaluate and return its single argument 
      ("quote-node" ,now.quote-node)

      ;; Just for testing, to be removed later.
      ("knum"   ,now.knum)
      ("delegator" ,now.delegator)
    )))

(define (extend-scope-with-builtin-constants scope-to-extend)
  (for-each
    (lambda (info) (apply now-add-const (cons scope-to-extend info)))
    `(
      ("false"  "Bool" ,now.false)
      ("true"   "Bool" ,now.true)
    )))

(define (extend-scope-with-builtin-types scope-to-extend)
  ;; Placeholder strings for now
  (for-each
    (lambda (info)
      (extend-scope-with-form-info scope-to-extend "Type" info))
    `(
      ("Int"     "Int")
      ("Uint"    "Uint")
      ("String"  "String")
      ("Proc"    "Proc")
      ("Fn"      "Fn")
      )))

(define (extend-scope-with-builtins scope-to-extend)
  (extend-scope-with-builtin-forms-proc scope-to-extend)
  (extend-scope-with-builtin-forms-fn   scope-to-extend)
  (extend-scope-with-builtin-types      scope-to-extend)
  (extend-scope-with-builtin-constants  scope-to-extend))

(extend-scope-with-builtins top-scope)
