;
;; Tokeniser
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
;; Temporary API
;;
; (define token-get-kind car)
; (define token-get-value cadr)

;;
;; TODO: when fetching numbers, we are currently using
;; string->number to convert them to internal SCHEME numbers.
;; Change this to keep them into some abstract data structure,
;; so they can be handled properly independently from the particular
;; scheme interpreter's numerical model
;;

;;
;; TODO: change functions below so that they do not take
;; 'stream' as argument, but only the top one does?
;; The other take it as part of the environment?
;;

;;
;; TODO: standardise naming convention,
;; use read-* instead of fetch-* ?
;;

;;;;
;;
;; Tokens are not expected to be exposed, they
;; count as 'internals'. We will expose AST nodes
;; after parsing.
;;
;;;;

;;
;; TODO: change the simple name 'stream' to 'cstream' or 'char-stream'
;;

;;
;; syntactic line terminator,
;; distinct from the physical line terminator
;; (char-eol?) defined elsewhere
;;

(define (char-line-terminator? c)
  (or (char-eol? c)
      (char=? c #\;)))

(define (fetch-line-terminator char-instream)
  (let
    ((terminator-char (instream-read-item char-instream)))
    (assert (char-line-terminator? terminator-char))
    `(eol ,terminator-char)))

;;
;; Whitespace
;;

(define (char-noneol-whitespace? c)
  (if (char-eol? c)
      #f
      (char-whitespace? c)))

(define (discard-nonterminating-whitespace char-instream)
  (let ((c (instream-peek-item char-instream)))
    (cond
      ((end-of-chain? c)
       #f) ;; return value does not matter. Just leave.
      ((char-noneol-whitespace? c)
       (instream-read-item char-instream) ;; Just Discard it
       (discard-nonterminating-whitespace char-instream)))))

;;
;; Comments
;;

(define (char-starts-comment? c)
  (char=? c #\#))

(define (discard-until-eol char-instream)
  (let ((c (instream-peek-item char-instream)))
    (if (or (end-of-chain? c)
            (char-eol? c))
       #f ;; return value does not matter. Just leave.
       (begin
         (instream-read-item char-instream)
         (discard-until-eol char-instream)))))

(define (discard-comment char-instream)
  (let ((c (instream-read-item char-instream)))
    (if (char-starts-comment? c)
      (discard-until-eol char-instream)
      (error "Internal error: discard-comment called on a non-comment character!"))))
    
;;
;; Special characters
;;
(define special-chars '(
  ( #\( left-round   )
  ( #\[ left-square  ) 
  ( #\{ left-curly   )

  ( #\) right-round  )
  ( #\] right-square ) 
  ( #\} right-curly  )

  ( #\. dot          )
  ( #\: colon        )
))

;;
;; Dot (access separator)
;;
(define (char-dot? c)
  (char=? c #\.))

;; TODO: double-check that the scheme 
;; convention for predicates is that
;; a name ending with '?' implies a
;; boolean result. If not, one can
;; optimize and return a list directly.
;;
;; TODO: maybe move the following function(s?)
;; elsewhere?
(define (char-special? c)
  (any->boolean (assoc c special-chars)))

;; Note: it is safe to use 'assoc-ref'
;; because #f is not associated with
;; any key, so it unambiguously means that
;; the key was not found.
(define (fetch-special-char char-instream)
  (let* ((c (instream-read-item char-instream))
         (token (assoc-ref special-chars c)))
    (if token
      token
      (error (string-append
               "INTERNAL ERROR: expecting a special char, got: '"
               (list->string (list c))
               "' ( code = "
               (number->string (char->integer c))
               ") instead")))))
;;
;; Numbers
;;

;;
;; Let's start simple. Just integers.
;; Floating point will be left for later.
;;
;; TODO: now number-strings are converted to scheme numbers
;; using string->number. Update to a more sophisticated approach?
;;

(define (valid-initial-number-character? c)
    (or (char-numeric? c)
        (char=? c #\-)))
    ; For now; later on numbers may start with
    ; non-numeric characters? Like, .3, for instance,
    ; or -1 ?
    ; IMPORTANT: it should theoretically be possible to decide
    ; whether something is an OPTION or a NUMBER

(define (fetch-number char-instream)

  ;; Simple implementation for now (snarfing
  ;; with a little checking on top).
  ;; TODO: add proper checks for target range, etc.
  ;; Make sure the implementation language can handle
  ;; all used integers properly.
  (define (string->now-int s)
    (let ((n (string->number s)))
      (assert (exact? n))
      (assert (integer? n))
      n))

  (define (fetch-number-tail already-fetched-r) ;; '-r' -> 'reversed'
    (let ((peeked-char (instream-peek-item char-instream)))
      (if (char-numeric? peeked-char)
        (fetch-number-tail (cons (instream-read-item char-instream) already-fetched-r))
        (reverse already-fetched-r))))
  ;;
  ;; Main body
  ;;
  (let* ((number-head (instream-read-item char-instream))
         (number-tail (fetch-number-tail '())))
    (assert (valid-initial-number-character? number-head))
    (list 'int (string->now-int (list->string (cons number-head number-tail))))))

;;
;; TODO: implement escape chars
;;

(define (fetch-string char-instream)

  (define (fetch-string-helper-start-quoted already-fetched-r)
    (let ((peeked-char (instream-peek-item char-instream)))
      (if (end-of-chain? peeked-char)
        (token-stream-error char-instream "While reading a string: file cannot end with a quote")
        (fetch-string-helper (cons (instream-read-item char-instream) already-fetched-r)))))

  (define (fetch-string-helper already-fetched-r) ;; '-r' -> 'reversed'
    (let ((peeked-char (instream-peek-item char-instream)))
      (cond
        ((char=? peeked-char #\\)
         (instream-read-item char-instream)
         (fetch-string-helper-start-quoted already-fetched-r))
        ((char=? peeked-char #\")
         (instream-read-item char-instream)
         (list 'string (list->string (reverse already-fetched-r))))
        (else
          (fetch-string-helper (cons (instream-read-item char-instream) already-fetched-r))))))

  ;;
  ;; Main body
  ;;
  (let ((string-starter-char (instream-read-item char-instream)))
    (if (char=? string-starter-char #\")
      (fetch-string-helper '())
      (error "INTERNAL ERROR: expecting string to start with double quotes. It does not."))))
;;
;; IDs
;;

(define non-alphanumeric-id-head-chars '(
    #\+
    ; #\- ; reserving for 'options'
    #\*
    #\/
    #\=
    #\>
    #\<
    #\_
))

(define (char-id-head? c)
  (or (char-alphabetic? c)
      (member? c non-alphanumeric-id-head-chars)))

(define (fetch-id char-instream)
  ;;
  ;; Private function definitions
  ;;
  (define non-alphanumeric-id-tail-chars
    (append '( #\- #\' )
            non-alphanumeric-id-head-chars))

  (define (char-id-tail? c)
    (or (char-id-head? c)
        (char-numeric? c)
        (member? c non-alphanumeric-id-tail-chars)))

  (define (fetch-id-tail already-fetched-r) ;; '-r' -> 'reversed'
    (let ((peeked-char (instream-peek-item char-instream)))
      (if (char-id-tail? peeked-char)
        (fetch-id-tail (cons (instream-read-item char-instream) already-fetched-r))
        (list 'id (list->string (reverse already-fetched-r))))))
  ;;
  ;; Main body
  ;;
  (let ((peeked-char (instream-peek-item char-instream)))
    (if (char-id-head? peeked-char)
      (fetch-id-tail (list (instream-read-item char-instream)))
      (token-stream-error char-instream "Attempted to fetch ID starting with an invalid character."))))

;;
;; Intermediate refactoring step: 
;;
;; * keep old function which returns token as list
;; * create adapter function which turns list into record (UPDATE: seems like we've done this?)
;; * refactor parser to use record-tokens                 (UPDATE: seems like we've done this?)
;;
;; After this refactoring step is complete, does it make
;; sense to refactor the tokenizer to produce records in
;; the first place instead of going through a list phase?
;; (Maybe not?)
;;

(define (lexer-fetch-token-as-list char-instream)
  (let ((first-char (instream-peek-item char-instream)))
    (cond
      ((end-of-chain? first-char)
       '(eof))

      ((char-line-terminator? first-char)
       (fetch-line-terminator char-instream))

      ((char-whitespace? first-char) ;; We have already handled the special case 'newline'.
       (discard-nonterminating-whitespace char-instream)
       '(whitespace)
       ;(lexer-fetch-token char-instream)   ;; Loop instead of 'whitespace'
       )

      ((char-starts-comment? first-char)
       (discard-comment char-instream)
       (lexer-fetch-token-as-list char-instream))

      ((char-id-head? first-char)
        (fetch-id char-instream))

      ;; For the future: make sure that valid
      ;; initial number characters (like "-"
      ;; for a negative number) do not interfere
      ;; with other kinds of tokens (like options
      ;; "-quiet"). See end of procedure for further
      ;; comments.
      ((valid-initial-number-character? first-char)
       (fetch-number char-instream))

      ((char=? first-char #\")
       (fetch-string char-instream))

      ((char-special? first-char)
       (fetch-special-char char-instream))

      (else
        (token-stream-error char-instream
                            (string-append "Unexpected character in stream: '"
                                           (char->string first-char)
                                           "'")))))) ;; Implicitly imply the error is at the CURRENT position.
      ;;
      ;; Maybe we could...
      ;;
      ;; 1) make "-" a separate token,
      ;; and the parser will recognise either the
      ;; sequence "-" "number" or the sequence
      ;; "-" "id" ?
      ;;
      ;; 2) Or, more simply, recognising '-' as special
      ;; case (ambiguous), and have a function called
      ;; 'parse-minus' parse the rest?
      ;;
      ;; It must, of course, be tested before
      ;; 'valid-initial-number-character?'
      ;;
      ;; 3) Not use "-" for options, use a different
      ;; character (but that would be counterintuitive
      ;; for people used to the shell or TCL)
      ;;

;;;;
;;
;; Main interface for other files
;;
;;;;


;;
;; Fetch tokens
;;

(define (lexer-fetch-token char-instream)
  (let* ((inchain (instream-get-inchain char-instream))
         (location (inchain-get-location inchain))
         ; (loc-str (location->string location))
         (token-as-list (lexer-fetch-token-as-list char-instream))
         (token-kind (car token-as-list))
         (token-rest (cdr token-as-list))
         (token-value (if (null? token-rest)
                        '()
                        (car token-rest))))
    ; (display loc-str)
    ; (display " ")
    ; (write token)
    ; (newline)
    (make-token token-kind token-value location)))

;; Convenience function
;; to save time and ease
;; readability (but it's still
;; allowed by the interface to
;; read the kind field directly,
;; and compare it manually).
;; This function is not used in
;; this file, but exported.
(define (token-kind-is? tok kind)
  (eq? (token-get-kind tok) kind))

;; Is the following procedure
;; used anywhere?
(define (assert-token-kind tok kind)
  (unless (token-kind-is? tok kind)
    (error
      (string-append
        "Expecting token kind '" (symbol->string kind)
        "', got '" (symbol->string (car tok))
        "' instead."))))

; ;; List of constant kinds, to be used later
; ;; (for instance, to determine if an AST node
; ;; is a constant or an expression)
; (define lexer-constant-kinds
;   '(int string))
