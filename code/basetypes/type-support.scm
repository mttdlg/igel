;;
;; Extra support functions for
;; dealing with types
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
;; Will evaluate an IGEL now.bool value
;; and convert it to a scheme boolean.
;; Will be used, for instance, in the
;; implementation of flow control
;; constructs.
;;
;; At the current stage in the implementation,
;; primitive IGEL types are mapped /directly/
;; to scheme values already, so no conversion
;; is necessary and the function is trivial.
;;
;; However, this will change in the near future.
;;
(define (bool-value-true? igel-bool)
  ; (assert (boolean? igel-bool))
  igel-bool)

