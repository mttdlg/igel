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
;; Note: the codebase has not been tested with chibi-scheme in quite some time.
;; This target has to be considered 'unmantained'
;;

(import (scheme small))
(import (srfi 69)) ; hashes

(define error-and-exit error) ; untested
;; (define (command-line-arguments (cdr (command-line)))) ; untested, probably requires an import. See r7rs for details.
;; Also... we use *script-arguments* now. Adapt.
;; We might need a 'print' command? Maybe?

(load "./platform/platform-generic.scm")
(load "./top-pi.scm")
