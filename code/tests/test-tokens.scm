;;
;; Test for the tokeniser
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
;; TODO: fix relative paths. Right now we must
;; express paths with respect to the top file.
;; We want to express them relative to the current
;; directory.
;;

; (load-ct "./tokens.scm")

(define (token-loop s) 
  (let* ((t (lexer-fetch-token s))
         (t-kind (token-get-kind t))
         (t-val (token-get-value t)))
    (write (cons t-kind
                   (cond
                     ((list? t-val) t-val)
                     (else
                       (list t-val)))))
    (cond
      ((eq? t-kind 'eof) t) 
      (else
        (if (eq? t-kind 'string)
          (begin
            (display " (prints as: [")
            (display t-val)
            (display "])")))
        (newline)
        (token-loop s)))))

(define (test-tokeniser-with-file-name file-name)
  (let ((instream (make-instream-from-file file-name)))
    (token-loop instream)))

(define (do-test-tokeniser list-of-files)
  (for-each test-tokeniser-with-file-name list-of-files))
