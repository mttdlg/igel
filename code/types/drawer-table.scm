;;
;; IGEL
;;
;; The "drawer table" data structure
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
;; A key/value ID table where each element
;; can be used as an lvalue (can be var/const, etc.)
;;

;;
;; TODO: affixes like '-now-', '-fresh-', etc. have been growing inconsistently
;; during development. Streamline and standardize.
;;

;;
;; NOTE: drawer-table is a STORAGE PRIMITIVE (for the
;; 'data' field of an igel-value), it is NOT a type
;; or an igel-value already!
;;
;; 'Struct' will be a now-type, and that one
;; will /use/ now-object for implementation
;; purposes.
;;
;; NOTE: it is FORBIDDEN (by convention) to store anything but
;; drawers into a drawer-table's htable. Some functions will
;; return either a drawer in case of success, or #f in case of
;; failure, so '#f' in particular is a reserved value, not valid
;; contents to return when, say, looking up a key.

(define-record-type
  <drawer-table>
  (make-drawer-table-private htable is-locked)
  drawer-table?

  (htable    drawer-table-get-htable)
  (is-locked drawer-table-get-is-locked drawer-table-set-is-locked-private!))
    ; is-locked: can we add/remove fields? It's like 'w' permission for a *nix directory.
    ; For hierarchical data structures, is-locked does not control write permission recursively.
    ; A topic is still open and needs to be decided upon: should the value of is-locked
    ; control whether we are allowed to /change/ the value of a slot? I.e., use 'set' on it?
    ; The current working concept is that is-locked does NOT control this detail, the whole
    ; responsibility falls on the specific drawer's setter. If one wants to lock
    ; the creation/removal of drawers AND forbid changing the value of drawers, the two
    ; operations have to be handled separately at idtable and drawer level.

;;
;; Misc convenience definitions
;;

; (define (drawer-table-try-setting-meta drawer-table value)
;   ; Returns #t on success (not locked)
;   ; returns #f on failure
;   (cond
;     ((drawer-table-get-is-locked drawer-table) #f)
;     (else
;       (drawer-table-set-meta-private! drawer-table value)
;       #t)))

;
; TODO: below, replace '...-if-exists' with  '...-if-key-exists' ?
; no, -get-if-exist -> -try-getting
;

(define (htable-add-new-drawer! htable key drawer)
  ;
  ; We just add a drawer associated with a not-yet-existing
  ; key in a hash table. This works with ANY hash table,
  ; not just the ones which are part of a drawer table
  ; (although this will be the main use case). Since we
  ; just deal with a hash table, we need not check is-locked
  ; (because there is none for a simple hash table)
  ;
  (assert (not (hash-table-exists? htable key))) ;; TODO: raise exception
  (hash-table-set! htable key drawer))

(define (drawer-table-add-new-drawer! dtable key drawer)
  (assert (not (drawer-table-get-is-locked dtable))) ; TODO, check type, check locked, now.Exception
  (let ((htable (drawer-table-get-htable dtable)))
    (htable-add-new-drawer! htable key drawer)))

(define (drawer-table-key-exists? drawer-table key)
  (hash-table-exists? (drawer-table-get-htable drawer-table) key))

(define (drawer-table-try-getting-drawer drawer-table key)
  (let ((htable (drawer-table-get-htable drawer-table)))
    (and (hash-table-exists? htable key);; -> '#f' if key does not exist
         (hash-table-ref htable key)))) ;; -> by convention, if we get here it is not #f

(define (drawer-table-get-drawer drawer-table key)
  (let ((result (drawer-table-try-getting-drawer drawer-table key)))
    (if result
        result
        (error (string-append "Key '" key "' does not exist!"))))) ;; TODO: exception

(define (drawer-table-get-value-with-drawer-accessor drawer-accessor drawer-table key)
  (let ((drawer (drawer-accessor drawer-table key)))
    (and drawer 
         (let ((getter (drawer-get-getter drawer)))
           (getter))))) ;; Remember: drawer always contains an igel-value,
                        ;; cannot contain #f directly.

(define (drawer-table-try-getting-value drawer-table key)
  (drawer-table-get-value-with-drawer-accessor drawer-table-try-getting-drawer
                                               drawer-table key))

(define (drawer-table-get-value drawer-table key)
  (drawer-table-get-value-with-drawer-accessor drawer-table-get-drawer
                                               drawer-table key))
;;
;; Constructors
;;

(define (make-fresh-drawer-table)
  (make-drawer-table-private (make-hash-table) #f))

(define (make-fresh-drawer-table-from-pairs is-locked pairs)
 (make-drawer-table-private
   (key-value-pairs->hash-table pairs)
   is-locked))

;
; It is vital that if the set operation fails, WE DO NOT LEAVE
; ANY DATA STRUCTURE IN AN INCONSISTENT STATE.
; Ideally, we should probably go with the stricter requirement
; that no state SHALL be changed at all.
;
; At first, though, the concept will be relaxed, and we say that
; no state SHOULD be changed, allowing exceptions in case there
; is a very specific, very good reason to change state.
;
; However, it is not guarranteed that this possibility will remain
; in the long term.
;
(define (drawer-table-try-setting! dtable key value)
  (let ((drawer (drawer-table-try-getting-drawer dtable key)))
    (if drawer
        (let ((setter! (drawer-get-setter drawer)))
          (setter! value)
          value) ; not allowed to be #f
        #f))) ;; TODO: now.exception

;(define (make-now-drawer-table-simple-value is-locked . list-of-entries)
;  (let* ((dtable (make-fresh-drawer-table #f))
;         (htable (drawer-table-get-htable dtable)))
;    (for-each
;      (lambda (entry)
;        (bind-list (access-policy key drawer-type value) ;; TODO: maybe-value, make it optional
;          (htable-add-new-drawer! htable key (make-now-drawer-simple drawer-type value))))
;      list-of-entries)
;    (drawer-table-set-is-locked-private! is-locked)))

(define (make-now-drawer-table-simple-from-data is-locked list-of-defs)
  (let* ((dtable (make-fresh-drawer-table))
         (htable (drawer-table-get-htable dtable)))
    (for-each
      (lambda (entry)
        (assert (= 4 (length entry)))
        (bind-list (access-policy key drawer-type drawer-data) entry ;; TODO: maybe-value, make it optional
          (let ((value (make-igel-value drawer-type drawer-data)))
            (htable-add-new-drawer! htable key (make-now-drawer-simple access-policy drawer-type value)))))
      list-of-defs)
    (drawer-table-set-is-locked-private! dtable is-locked)
    dtable))
;
; TODO/FIXME: getting/setting items from drawer table (and checking if keys are there), hash-table-style
;
; we have ' drawer-table-get-value-if-exists '.
; What about setting?

