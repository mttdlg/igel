
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;;   IGEL                   ;;
;;                          ;;
;;   Core Semantic layer    ;;
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
;; Interpreter support part:
;;
;; data structures,
;; name resolution,
;; set, dot basics
;; and delegation.

(load-ct "terp/scopes.scm")
(load-ct "terp/form-support.scm")
(load-ct "terp/set-support.scm")
(load-ct "terp/dot-support.scm")

;;
;; now-Interpreter:
;; main execute/eval logic
;;
(load-ct "terp/execute-form.scm")
(load-ct "terp/eval.scm")
(load-ct "terp/now-terp.scm")

;;
;; now-interpreter: syntax extensions
;;
(load-ct "terp/parse-decl.scm") ;; Are type signature constructs part of the basic syntax, or the specific languge?
                                ;; For now, we consider them 'syntax extensions' to the 100% guarranteed common core,
                                ;; and postpone the decision to later.
                                ;; Note that they can be considered syntax extensions only if there is a way to
                                ;; add them to the base language. Define what "there is a way" means. Who should
                                ;; "provide a way"? The default 'later' language? The now-language?

;
; We now have the basic machinery of the interpeter in place.
; Time to fill its data structures with built-ins.
;

;;
;; now-language builtins
;;
(load-ct "terp/builtin-pforms.scm") ; <-- BOOKMARK: this is the next file to review.
(load-ct "terp/form-tables.scm")
