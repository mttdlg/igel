;;
;; join.scm
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
;; Joins a list of strings with a separator.
;; It is mandatory to pass the separator as argument.
;;

(define (join-with-separator list-of-strings separator)
  (let join-loop
    ((list-of-strings list-of-strings)
     (accumulate-r '()))
    (cond
      ((null? list-of-strings)
       (apply string-append (reverse accumulate-r)))
      ((null? (cdr list-of-strings))
       (join-loop '()
                  (cons (car list-of-strings) accumulate-r)))
      (else
        (join-loop (cdr list-of-strings)
                   (cons (string-append (car list-of-strings) separator)
                         accumulate-r))))))

;;
;; Join a list of strings with a separator.
;; If the separator is not specified, a single space is the default.
;;
(define (join . l)
  (let ((arg-count (length l)))
    (case arg-count
      ((1) (join-with-separator (car l) " "))
      ((2) (apply join-with-separator l))
      (else
        (error (string-append "Invalid number of arguments to 'join': " (number->string arg-count)))))))
