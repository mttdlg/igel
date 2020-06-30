;;
;; Top file for TinyScheme
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
;; Official implementations missing at the moment:
;; SRFI  9 ; define-record-type
;; SRFI 69 ; make-hash-table -> official implementtion found, but:
;;                              * depends on SRFI 9
;;
;; For now, we use a 'faked' version of SRFI 9/69 with
;; just enough compatibility to make things work.
;;
;; TODO: see if there is a suitable replacement?
;;

; (tracing 1)

; (gensym)
(load "./platform/platform-tinyscheme.scm")
(load "./top-pi.scm")
