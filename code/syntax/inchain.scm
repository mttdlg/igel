;;
;; Input Chains
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
;;
;; TODO: provide way to explicitly close
;; the port associated with an inchain.
;;

;;
;; TODO: 'stream' is already a technical term
;; (SRFI 41) -- rename our object, and/or
;; actually use a SRFI-41 stream to 
;; implement it.
;;
;; UPDATE: SRFI-41 'stream's seem a good
;; fit for what we're trying to do. They
;; seem to refer to R6RS, tho', so
;; double-check how portable/available
;; they are in the interpreters we support.
;; However... are they too complicated?
;; Use something simpler?

;; Temp placeholder for stream system:
;; A cstream is just a port.
;; stream-* functions are redirected
;; to the regular stream functions.

;;
;; This is okay for lexer. But parser will need something more
;; sophisticated. The idea would be to use the same interface
;; (peek/read) but to provide parsed tokens instead.
;; Perhaps, as a start, work should be split?
;; First attempt: create a list of tokens
;; which represents the whole file. 
;; then create a stream based on this list, which will
;; be encapsulated inside the 'stream' object for the
;; parser.
;;

;; TODO: replace it with a stream handling system
;; which can, in case of error, report at which file,
;; line, column the error was encountered.
;; In fact... should it be part of the token,
;; in case the error is found by the parser?
;; Some sort of association list record?
;;
;; Also, keep debugging into account!
;;

;;
;; Addition: interface should provide,
;; on request, a 'position object'.
;; Position object can be used for error
;; reporting or for rewinding (backtracking)
;;
;
;
; (char-stream-peek-char cstream)
; (char-stream-read-char cstream)
; (char-stream-get-pos cstream)           ;; Return opaque object that represents position in char-stream. Will be used for backtracking.
; (char-stream-set-pos cstream pos)       ;; Rewind stream to provided point (pos knows which 'stream' it comes from, somehow, but both must be provided)
; (char-stream-pos-repr pos)              ;; Return printable representation of position, for instance to use it in error messages.
;


;;
;; TODO: change interface:
;;
;; (inchain-get-item chain) : returns CURRENT char in inchain (replaces inchain-peek-char)
;; (inchain-get-next chain) : returns next position in inchain (one needs to apply "inchain-get-char" to it to get to the character)
;; (inchain-error    chain) : reports an error at the specified "chain" position
;;

;;
;; WARNING: needs more testing!
;;

;;
;; To think about: is it possible to implement the following
;; using promises?
;;

(define-record-type <location>
                    (make-location id-string column line)
                    location?
                    (id-string location-get-id-string)
                    (column location-get-column)
                    (line location-get-line))

(define (starting-location filename)
  (make-location (string-append "file \"" filename "\"") 1 1))

(define (next-location location char)
  (let ((id-string  (location-get-id-string location))
        (column     (location-get-column    location))
        (line       (location-get-line      location)))
    (if (char-eol? char)
      (make-location id-string 1 (+ 1 line))
      (make-location id-string (+ 1 column) line))))

;;
;; @fun location->string
;;      Turns location information into a string, to pinpoint
;;      the exact point in the stream
;;
;; @arg location: location
;;      Location to extract the string form from 
;;
;; @ret : string
;;      A string with an informational message about the location
;;
(define (location->string location)
  (string-append (location-get-id-string location)
                 ", line "
                 (number->string (location-get-line location))
                 ", column "
                 (number->string (location-get-column location))))

;; @fun location->error-prefix
;;      Return the kind of string one expects to see before error messages
;;      (something like "In file ABC, line Y, column X: ")
;;
;; @arg location : location
;;      Location to produce the string from
;;
;; @ret : string
;;      A helpful string that can be used as a prefix for error reporting.
;;      It will report information like filename (or, 'translation
;;      unit', line, column).
;;
(define (location->error-prefix location)
  (string-append "In " (location->string location) ": "))

(define-record-type <inchain-end-of-chain-type>
                    (make-end-of-chain)
                    end-of-chain-type?)

;; Let us create a singleton
(define end-of-chain (make-end-of-chain))
(define (end-of-chain? candidate) (eq? candidate end-of-chain))

;;
;; TODO: replace the word 'internal' with the word 'private'
;; (re-align indentation accordingly)
;;
(define-record-type <inchain>
                    (make-inchain-private item next-private location)
                    inchain?
                    (item inchain-get-item inchain-set-item!)
                    
                    ;; Private part of the interface:
                    (next-private inchain-get-next-private inchain-set-next-private!)

                    ;; Public part again (TODO: move private/public together, enen
                    ;; in arguments to constructor )
                    (location inchain-get-location)) ; filename: line, column

;; @public
;; @fun inchain-get-next
;;      ...
;;
;; @arg inchain : inchain
;;      ...
;;
;; @ret : ...
;;      ...
;;

;;
;; Inchains get their input from a thunk which returns a new item
;; at each call, and returns the special singleton ' end-of-chain '
;; to signal that there are no more items to read.
;;
;; Inchains are not limited to characters, but can contain
;; other types as well.
;;

;;
;; Publically available "method" to access the next item:
;;
(define (make-read-char-thunk-from-in-port in-port)
  (lambda ()
    (let ((next-char (read-char in-port)))
      (cond
        ((eof-object? next-char)
         (close-input-port in-port)
         end-of-chain)
        (else next-char)))))


;;
;; TODO: for readability, don't use 'next' twice,
;; use 'new', 'old' (if needed) and 'next'
;;
(define (inchain-get-next inchain)
  ;;
  ;; Re-align end-of-file semantics with rest of
  ;; code. Is it okay to return #f , or perhaps '(),
  ;; or should it return an infinite stream of valid
  ;; chains, all with an EOF object? Raise an error?
  ;; Hm. Probably 'raise an error'.
  ;;
  (let ((next-inchain-or-read-item-thunk (inchain-get-next-private inchain)))
    (cond
      ((procedure? next-inchain-or-read-item-thunk)
       (let* ((read-item-thunk next-inchain-or-read-item-thunk) ;; for readability
              (location (inchain-get-location inchain))
              (next-item (read-item-thunk))
              (next-next-private read-item-thunk)
              (updated-next-private (make-inchain-private next-item
                                                          next-next-private
                                                          (next-location location
                                                                         (inchain-get-item inchain)))))
         (inchain-set-next-private! inchain updated-next-private)
         updated-next-private))
      ((inchain? next-inchain-or-read-item-thunk)
       next-inchain-or-read-item-thunk)
      ((end-of-chain? next-inchain-or-read-item-thunk) ; next-private is end-of-chain
       (error "Attempted to read an inchain past its end!"))
      ; ((eof-object? next-inchain-or-read-item-thunk)
      ;  (inchain-error inchain "EOF-object not expected in inchain!"))
      (else (inchain-error inchain "Unexpected condition")))))

(define (inchain-error inchain msg)
  (error (string-append
           (location->error-prefix (inchain-get-location inchain))
           msg)))

;;
;; Constructors go here:
;;
(define (make-inchain-from-input-thunk reader-thunk unit-name)
  (let* ((read-item-thunk reader-thunk)
         (next-item (reader-thunk)))
    (make-inchain-private next-item
                          read-item-thunk
                          (starting-location unit-name))))

(define (make-inchain-from-input-port input-port port-name)
  (make-inchain-from-input-thunk (make-read-char-thunk-from-in-port input-port)
                                 port-name))

(define (make-inchain-from-file filename)
  (if (not (file-exists? filename)) ;; R6RS function... how portable is it?
    (error (string-append "Attempted to open non-existing file \""
                          filename "\".")))
  (let ((in-port (open-input-file filename)))
    (make-inchain-from-input-port in-port filename)))

;;
;; TODO: remove this old interface.
;; Maybe attempt to implement it in terms
;; of the new interface, first, to ease
;; refactoring?
;;
;; 'instream', at the beginning will pretty much
;; be equivalent to a box
;; which contains an inchain, and which
;; will be updated by the commands.
;;
;; To support encapsulation, however,
;; we do not expose the fact that it is pretty much
;; a box, because we might extend the structure
;; in the future.
;;
(define-record-type <instream>
  (make-instream-private inchain)
  instream?
  (inchain instream-get-inchain instream-set-inchain-private!))

;;;
;;; The following two APIs are untested.
;;;

;;
;; instream->inchain API
;;

(define (make-instream-from-file filename)
  (make-instream-private (make-inchain-from-file filename)))

(define (make-instream-from-input-port input-port port-name)
  (make-instream-private (make-inchain-from-input-port input-port port-name)))

(define (instream-peek-inchain s)
  (instream-get-inchain s))

(define (instream-read-inchain s)
  (let* ((old-inchain (instream-get-inchain s))
         (new-inchain (inchain-get-next old-inchain)))
    (instream-set-inchain-private! s new-inchain)
    old-inchain))

;;
;; file-like API?
;; Shall we use this at all?
;;
(define (instream-peek-item s)
  (inchain-get-item (instream-get-inchain s)))

(define (instream-read-item s)
  (let* ((new-inchain (instream-read-inchain s)))
    (inchain-get-item new-inchain)))

(define (instream-error s msg)
  (let* ((inchain (instream-get-inchain s))
         (location (inchain-get-location inchain))
         (error-prefix (location->error-prefix location)))
    (error (string-append error-prefix msg))))

;; Again: to keep things simple, for now we seaparate the token stream
;; from the character stream.
