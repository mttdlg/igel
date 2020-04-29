;;
;; now.Object
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
;; TODO: change file name to now-object.scm
;; TODO: change table entries to have three fields:
;; * name
;; * typecheck, for setting -> must be function object (type-lock)
;;   (or should we just store variable 'type', so that it can be queried?)
;;   (should we set a 'type', and the 'type' determines the 'typecheck'?)
;;   Probably go with 'type', allows for formal checks later. Go for what it /is/,
;;   not what it /does/.
;; * value (with latent type)
;;
;; Object
;; (do we need Classes?)
;;
;; To address diamond-problem:
;; * Separate interfaces and implementation.
;; * Provide 'reasonable defaults' when non-ambiguous,
;;   but do not when there is ambiguity.
;; * Diamond problem: 'interface' will be automatically
;;   populated with non-ambiguous methods. Ambiguous methods
;;   will be left dangling. Attempting to call them will produce
;;   an error (almost certainly at compile time)
;;      -> solve it by distinguishing 'unit' and 'integration'?
;; * When 'compiling to C++;, interfaces for non-compatible objects
;;   could be implemented as shims.
;; * Maybe one could borrow from the TLM here, and 'connect' interfaces,
;;   so that they're just function pointers stored in an array. Multiple levels
;;   of indirection, if purely static, will just return the pointer to the object,
;;   not a whole chain of pointers.
;;
;; class foobar {
;;  interface {
;;      inherit Foo except {
;;          diamond-method
;;      } -> foo-if
;;      inherit Bar except {
;;          diamond-method
;;      } -> bar-if
;;      
;;     take shared-method from bar
;;  }
;;
;;  implementation {
;;      state foo [Foo] implements foo-if
;;      state bar [Bar] implements bar-if
;;
;;      method do-a-thing
;; }
;;
;;
;; Shorten the name to 'nobj' or just 'obj'
;; when used as part of function and variable
;; names?
;;

;;
;; Structure: each 'member' of an object will have an associated 'drawer'
;; which represents extra data associated with the entry in the symbol table.
;;
;; Implementation:
;;
;; [Object]<>---[ hash table ]
;;
;; Each entry in the [hash table] contains a [ drawer ] (old 'valbox'), which
;; in turn will contain the actual data member (or method) item.
;; drawer is native scheme type in this implementation.
;; There will be separate methods of the 'object' class for accessing
;; drawers and the members themselves.
;;

;;
;; Current concept: we do not adhere to a hierarchical object-oriented
;; system a la C++, but we provide the primitives to build one.
;;
;; GenericObject will be the basis for this kind of object orientation
;;
;; ALTERNATELY:
;;
;; We distinguish between PRIMITIVE TYPES (no inheritance between them)
;; and OBJECT HIERARCHY.
;;
;; An 'int' is not an object.
;; An Int /is/ an object. It is implemented as an Object containing an int and extra stuff.
;;
;; See how Python's Classic Classes worked.
;;
;; Another possibility:
;;  ID table entries (drawers) can only contain 'custom objects' (old now.object's).
;;  Specific fields inside objects can contain primitive/native types. Attempting to
;;  use these primitives from within the interpreted language will create a corresponding
;; 'custom object' on the fly, like we do with syntax types.
;;
;; ---
;;
;; How to call anything that can be stored in a variable?
;; Up to now, we have called it now.object, but is this appropriate?
;; Maybe it should be now.data ?
;;


;;
;; experimental/scratch code start
;;


;;
;; For ALL now-objects (not just those deriving from GenericObject)
;;
;; TODO: should this REPLACE the drawer/valbox structure?
;;
;; Current:
;;
;; { namespace : "name" <drawer> | "name" <drawer> | "name" <drawer> }
;; { drawer    : type   <type>   | getter <getter> | setter <setter> }
;; { type      : ... (driver functions) }
;;
;; Maybe:
;; { namespace : "name" <type> | "name" <type> | "name" <type> }
;; { type      : data   <data> | ... (driver functions including get/set) ...  }
;; ^^ Question about this approach: how about const qualifier?
;;

;;
;; experimental/scratch code end
;;

(define-record-type
  <now-object>
  (make-now-object kind htable)
  now-object?
  (kind     get-now-object-kind)
  (htable   get-now-object-hash-table))

;;
;; TODO: what 'drawer' to use? Constant or variable?
;;       Should we delegate to caller, so that each
;;       entry will need to be explcitly defined?
;;       Come up with specific syntax? Something like...
;;
;; (make-raw-now-object-from-pairs "Foo"
;;      '((bar (var   now-bar)
;;        (baz (const now-baz)))))
;;

(define (make-raw-now-object-from-pairs kind pairs)
  (make-now-object kind (key-value-pairs->hash-table pairs)))


;;
;; Next: rework the following method:
;; * Code that uses it will almost certainly access the
;;   contents of the drawer immediately after. Update said code.
;;

(define (now-object-has-member? now-obj name)
  (hash-table-exists? (get-now-object-hash-table now-obj) name))

(define (get-now-object-raw-member now-obj name)
  (assert (now-object? now-obj))
  (hash-table-ref (get-now-object-hash-table now-obj) name))

;(define (set-now-object-raw-member! now-obj name val)
;  (assert (now-object? now-obj))
;  (hash-table-set! (get-now-object-hash-table now-obj) name val))

(define (get-now-object-member now-obj name)
  (let* ((drawer (get-now-object-raw-member now-obj name))
         (getter (drawer-get-getter drawer)))
    (getter)))

;;
;; Support, helper function that can maybe moved elsewhere.
;;
(define (get-now-object-member-or-complain now-obj name)
  (if (now-object-has-member? now-obj name)
    (get-now-object-member now-obj name)
    ; TODO: replace with exception.
    (error (string-append "Could not access object member '" 
                          name
                          "'"))))


(define (get-now-object-drawer-member-or-complain now-obj name)
  (if (now-object-has-member? now-obj name)
    (let ((result (hash-table-ref (get-now-object-hash-table now-obj) name)))
      (assert (drawer? result))
      result)
    (error (string-append "Could not access object member '" 
                          name
                          "'"))))

