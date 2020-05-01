;;
;; Barebones approximation of SRFI-9
;; (mainly meant for TinyScheme)
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
;; Provide enough of a(n approximation of) a subset
;; of SRFI-9 to get the rest of the code runnig. Somehow.
;; The code will not be fully SRFI-9 compliant, and usage
;; that would expose the non-compliant parts shall be avoided.
;; (example of non-compliancy: we implement records as
;; vectors, and the procedure 'vector?' will return #t
;; when passed a record created with this implementation,
;; which violates the SRFI-9 specification. In our code,
;; therefore, we have to be aware of this and use precautions
;; if we want to support this approximation).
;;
;; NOTE: a possible approach to resolve the situation mentioned
;; above: 
;; * store a special 'gensym' symbol into index 0 of the vector
;;   instead of the symbol '_fake-9_
;;
;;     (define fake9-marker (gensym))
;;
;; * store the value of 'vector?' (procedure) as 'fake9-old-vector?'
;;
;;     (define fake9-old-vector? vector?)
;;
;; * redefine 'vector?' as
;;
;;     (define (vector? x)
;;       (if (fake9-old-vector? x)
;;         (and (>= (vector-length x) 1)
;;              (eq? (vector-ref x 0) fake9-marker))
;;        #f))
;;

;;
;; Current approach:
;; We do not care too much about higyene;
;; we assume the '_fake9_' prefix will not be used
;; by identifiers provided by the user
;; (these are not general purpose libraries, 
;; they are just a crutch to get our own code 
;; to somehow run under TinyScheme, for
;; bootstrapping purposes)
;;
;; In particular, we do not go out of our way to check consistency
;; (at first, we won't check consistency between the constructor
;; and the field specs, for instance).
;; We assume that the code has already been tested under some other
;; Scheme system which provides full SRFI-9 support, and we just want
;; to get the already tested code to somehow run under TinyScheme.
;;

(define (_fake9_make-vector-init vector-symbol constructor-args)
  (let enum-loop ((args constructor-args)
                  (index 2)
                  (accumulated '()))
    (if (null? args)
      accumulated ;; No need to reverse
      (enum-loop
        (cdr args) 
        (+ 1 index)
        (cons `(vector-set! ,vector-symbol ,index ,(car args)) accumulated)))))

(define (_fake9_define-for-record-type-constructor name constructor-prototype)
  (let* ((result (gensym))
         (constructor-arguments (cdr constructor-prototype))
         (vector-size (+ 2 (length constructor-arguments))))
    `(define ,constructor-prototype
       (let ((,result (make-vector ,vector-size)))
         (vector-set! ,result 0 '_fake9_)
         (vector-set! ,result 1 ',name)
         ,@(_fake9_make-vector-init result constructor-arguments)))))

(define (_fake9_define-for-predicate predicate-name record-name)
  (let ((record-param (gensym)))
    `(define (,predicate-name ,record-param)
       (and (vector? ,record-param)
            (>= (vector-length ,record-param) 2)
            (eq? (vector-ref ,record-param 0) '_fake9_)
            (eq? (vector-ref ,record-param 1) ',record-name)))))

(define (_fake9_define-for-accessor record-predicate accessor-name field-number)
  (let ((record-param (gensym)))
    `(define (,accessor-name ,record-param)
       (assert (,record-predicate ,record-param))
       (vector-ref ,record-param ,field-number))))

(define (_fake9_define-for-mutator record-predicate mutator-name field-number)
  (let ((record-param (gensym))
        (value-param  (gensym)))
  `(define (,mutator-name ,record-param ,value-param)
     (assert (,record-predicate ,record-param))
     (vector-set! ,record-param ,field-number ,value-param))))

(define (_fake9_defines-for-record-field predicate lst field-number)
  (let ((rest-of-list (cdr lst)))
       ;(field-name   (car lst))
    (case (length rest-of-list)
      ((0) '())
      ((1) (list
             (_fake9_define-for-accessor predicate (car  rest-of-list) field-number)))
      ((2) (list
             (_fake9_define-for-accessor predicate (car  rest-of-list) field-number)
             (_fake9_define-for-mutator  predicate (cadr rest-of-list) field-number)))
      (else (error "Invalid number of parameters for record field definition!")))))

(define (_fake9_defines-for-record-type args)
  (let ((name            (car args))
        (constructor-tpl (list-ref args 1))  ; TemPLate
        (pred            (list-ref args 2))
        (fields-list     (cdddr args)))
    (let fields-loop
      ((field-number 2)
       (remaining-fields-list fields-list)
       (accumulated-result
         (list
           (_fake9_define-for-record-type-constructor name constructor-tpl)
           (_fake9_define-for-predicate pred name))))
      (cond
        ((null? remaining-fields-list)
         `(begin ,@accumulated-result))
        (else (fields-loop (+ 1 field-number)
                           (cdr remaining-fields-list)
                           (append (_fake9_defines-for-record-field
                                     pred 
                                     (car remaining-fields-list)
                                     field-number)
                                   accumulated-result)))))))


(macro (define-record-type s-expr)
 (_fake9_defines-for-record-type (cdr s-expr)))

;;
;; TESTs (remove from final version)
;;

;; (define (writeln thing)
;;   (write thing)
;;   (newline))

; (writeln "Hello!")

;; (writeln (_fake9_defines-for-record-type '(pare (kons kar-f kdr-f) pare? (kar-f kar set-kar!) (kdr-f kdr set-kdr!))))
; (writeln (_fake9_define-for-record-type-constructor '(pare kar kons)))
; (writeln (_fake9_define-for-predicate 'foo? 'foo))
; (writeln (_fake9_define-for-accessor 'foo? 'get-kar 'kar))
; (writeln (_fake9_define-for-mutator 'foo? 'set-kar! 'kar))
