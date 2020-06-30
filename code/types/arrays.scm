;;
;; IGEL
;; 
;; now.array
;;
;; (C) 2020 Matteo De Luigi
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

(define now.array-type-id (gen-type-id))

(define (now.array-accepts-config? type-accepted type-offered)
  (let* ((state-typ-acc (type-get-state type-accepted))
         (state-typ-off (type-get-state type-offered)))
    (and (type-accepts-type?
            (igel-value-get-data (idtable-get-value state-typ-acc "type"))
            (igel-value-get-data (idtable-get-value state-typ-off "type")))
         ;; TODO: define now.uint comparison helper functions.
         ;; NOTE: we skip typecheck because we trust the code to be correct
         ;; and generate size of the correct type (data will be scheme integer)
         (= (igel-value-get-data (idtable-get-value state-typ-acc "size"))
            (igel-value-get-data (idtable-get-value state-typ-off "size"))))))

(define (now.array-default-data-constructor array-type-instance)
  (let* ((array-config ((type-get-state array-type-instance)))
         (cell-type (drawer-table-get-value array-config "type"))
         ;; NOTE: we skip typecheck because we trust the code to be correct
         ;; and generate size of the correct type (data will be scheme integer)
         (size      (drawer-table-get-value array-config "size"))
         (tmp-vector (make-vector (igel-value-get-data size)))
         (cell-default-data-constructor (type-get-default-data-constructor cell-type)))

    (let init-loop ((i 0))
      (if (>= i size)
        (make-igel-value (now.array-ptype cell-type size)
                         tmp-vector)
        (begin
          (vector-set! tmp-vector i (make-igel-value cell-type (cell-default-data-constructor)))
          (init-loop (+ 1 i)))))))
;;
;; Alternative approach to for init loop above (untested):
;;
;         (do ((i size (+ 1 i)))
;             ((>= i size) (make-igel-value (now.array-ptype cell-type size) tmp-vector))
;           (vector-set! tmp-vector i (make-igel-value cell-type (default-data-constructor))))

(define now.array-ptype
          ;;
          ;; Main body
          ;;
          (make-parametric-type 
            "now.array" 
            type-default-fallback-types
            now.array-type-id
            now.array-accepts-config? 
            (lambda (val) "<placeholder string for now.array>") ;; TODO: improve
            now.array-default-data-constructor
            (lambda (type size)
              ;; TODO: create idtable instead.
              (make-idtable-simple-from-data #t
                `((const "type" ,now.type ,type)
                  (const "size" ,now.int  ,size))))))

; (define (now.array-set now-array index igel-value)

;;
;; Support function for building now-types from internal scheme data structures:
;;

(define (vector->now.array type scheme-vector)
  (let ((size (vector-length scheme-vector)))
    (make-igel-value (now.array-ptype type size) scheme-vector)))

(define (list->now.array type scheme-list)
  (vector->now.array type (list->vector scheme-list)))

;; (define reverse-scheme-list-into-now.array ...)
