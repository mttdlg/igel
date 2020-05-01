;;
;; Test for now-language interpreter
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

; ; (define p (open-input-file (car *script-arguments*)))
; 
; (define (test-now-terp-on-file-name file-name)
; ;;   (display "Now interpreting ")
; ;;   (display file-name)
; ;;   (newline)
;     (let* ((char-instream (make-instream-from-file file-name))
;            (token-stream (make-token-stream-from-char-instream char-instream))
;            (ast (parse-unit token-stream)))
;       ;; (write ast)
;       ;; (newline)
;       (now-terp top-scope ast)))
; 
; (for-each test-now-terp-on-file-name *script-arguments*)
; ; (close-input-port p)

(define (test-now-terp-on-file-name file-name)
  (let* ((char-instream (make-instream-from-file file-name))
         (token-stream (make-token-stream-from-char-instream char-instream)))
    (parse-and-eval-statements top-scope token-stream)))

; (tracing 1)
(define (do-test-now-terp list-of-files)
  (for-each test-now-terp-on-file-name list-of-files))
