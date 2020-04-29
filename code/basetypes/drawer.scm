;;
;; Drawer
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

;; Should 'drawer be renamed to 'reference'? NO! It's a TABLE ENTRY!
;; maybe 'id-entry' for ID Table Entry
;; getter/setter -> deref, overwrite

(define-record-type
  <drawer>
  (make-drawer kind getter setter)
  drawer?
  (kind   drawer-get-kind)
  (getter drawer-get-getter drawer-set-getter!)
  (setter drawer-get-setter drawer-set-setter!))


;; Built-in drawers for variables and constants:

(define (make-now-drawer-var kind contents)
  (define (getter) contents)
  (define (setter val) (set! contents val))
  (make-drawer kind getter setter))

(define (make-now-drawer-const kind contents)
  (define (getter) contents)
  (define (setter val) (error "Attempted to assign a value to a now-constant"))
  (make-drawer kind getter setter))
