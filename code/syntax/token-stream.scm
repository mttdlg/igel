;;
;; Token Stream
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

;; Make sure that the input port char-stream takes input from is
;; eventually closed!

;;
;; Note: location is a property of the TOKEN, not of the TOKEN STREAM LINK!
;; Same goes for the 'inchain link' /after/ the token, to proceed after
;; parsing the token.
;; This way, backtracking can switch from token-mode to char-mode
;;
;; UPDATE: do not allow custom syntax at parser level, do not allow
;; backtracking. Provide a mechanism to slurp in whole blocks.
;; Return them as strings. Parse string using string function/methods
;; to access characters, do not meddle with the internals.
;;
;; Example:
;;
;; asm {:asm: 
;;      mov r0, 1
;;      mov r1, 2
;; loop:
;;      sub r3, 1
;;      jlt r3, r4, loop
;;      ret
;; :asm:}
;;
;; The whole block will be returned as a string and passed to something
;; like 'block_parsers.asm'
;;
(define-record-type <token-instream>
                    (make-token-stream-private char-instream lookahead)
                    token-stream?
                    (char-instream token-stream-get-char-instream-private)
                    (lookahead token-stream-get-lookahead-private token-stream-set-lookahead-private!))

;;
;; @fun create-token-stream-from-instream
;;        Creates a token-stream from a char-instream
;;
;; @arg instream : instream (providing characters)
;;        instream which returns characters
;;
;; @ret : token-stream
;;        an instream which
;;        returns tokens
;;
;; @side-effects
;;        Might affect the 'stream' input structure.
;;
(define (make-token-stream-from-char-instream char-instream)
  (make-token-stream-private char-instream (lexer-fetch-token char-instream)))

(define (tstream-peek-token token-stream)
  (token-stream-get-lookahead-private token-stream))

(define (tstream-read-token token-stream)
  (let* ((char-instream (token-stream-get-char-instream-private token-stream))
         (result (token-stream-get-lookahead-private token-stream))
         (lookahead (lexer-fetch-token char-instream)))
    (token-stream-set-lookahead-private! token-stream lookahead)
    result))

(define (token-stream-error token-stream msg)
  ; TODO: use char-stream-error or something instead
  ; and provide location
  (error msg))

