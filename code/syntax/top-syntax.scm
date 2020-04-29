
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;;       Syntax layer       ;;
;;                          ;;
;;  Takes source as input,  ;;
;;  produces AST as output  ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Expects the following files to have already been loaded:
;; - join.scm
;; - any-to-string.scm
;; - support.scm
;;

;;
;; TODO: fix relative paths. Right now we must
;; express paths with respect to the top file.
;; We want to express them relative to the current
;; directory.
;;

;;
;; Input streams:
;;
(load-ct "syntax" "inchain.scm")

;;
;; Tokeniser:
;;
(load-ct "syntax" "tokens.scm")
(load-ct "syntax" "token-stream.scm")

;;
;; Parser:
;;
(load-ct "syntax" "ast-support.scm")
(load-ct "syntax" "parser.scm")

