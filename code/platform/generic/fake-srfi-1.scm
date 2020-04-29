;;
;; Barebones replacement for SRFI-1
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
;; Provide barely enough an approximate implementation
;; of SRFI-1 to get the rest of the code working.
;; Somehow.

(define (drop clist i)
  ;;
  ;; The case where i is negative or fractional
  ;; is going to be a problem only if clist
  ;; does not terminate (for instance, looping on
  ;; itself). Good enough for a drop-in replacement.
  ;;
  (cond
    ((zero? i) clist)
    ((null? clist) (error "Invalid arguments passed to 'drop'"))
    ((drop (cdr clist) (- i 1)))))
