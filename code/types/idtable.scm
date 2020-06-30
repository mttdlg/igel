;;
;; IGEL
;; 
;; now.idtable
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


(define now.idtable
  (make-type "now.idtable"
             type-default-fallback-types
             type-no-config-check
             (lambda (x) "<placeholder for now.idtable>")
             (make-fresh-drawer-table)))

(define (now.idtable? x)
  (type-accepts-value? now.idtable x))

(define (idtable-key-exists? idtable key)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-key-exists? (igel-value-get-data idtable) key))

(define (idtable-try-getting-drawer idtable key)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-try-getting-drawer (igel-value-get-data idtable) key))

(define (idtable-get-drawer idtable key)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-get-drawer (igel-value-get-data idtable) key))

(define (idtable-try-getting-value idtable key)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-try-getting-value (igel-value-get-data idtable) key))

(define (idtable-get-value idtable key)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-get-value (igel-value-get-data idtable) key))

(define (make-idtable-simple-from-data is-locked list-of-defs)
    (make-igel-value
      now.idtable
      (make-now-drawer-table-simple-from-data is-locked list-of-defs)))

(define (make-fresh-idtable)
  (make-igel-value now.idtable (make-fresh-drawer-table)))

(define (idtable-get-is-locked idtable)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-get-is-locked (igel-value-get-data idtable)))

(define (idtable-set-is-locked-private! idtable is-locked)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-set-is-locked-private! (igel-value-get-data idtable) is-locked))

;
; TODO: idtable-try-adding-new-drawer!
;

(define (idtable-add-new-drawer! idtable key drawer)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-add-new-drawer! (igel-value-get-data idtable) key drawer))

(define (idtable-try-setting! idtable key value)
  (assert (now.idtable? idtable)) ; TODO: now.Exception
  (drawer-table-try-setting! (igel-value-get-data idtable) key value))

(define (idtable-set! idtable key value)
  (let ((outcome (drawer-table-try-setting! (igel-value-get-data idtable) key value)))
    (if outcome
      outcome
      (error (string-append "Could not set identifier '"
                            key
                            "' in idtable"))))) ; TODO: now.Exception ?
