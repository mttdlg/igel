;;
;; IGEL
;;
;; Support for objects
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
;; *** WARNING ***
;;
;; long list of notes/comments (possibly obsolete)
;; before the actual code.
;;

;
; TODO: make sure that the object interface can deal with
; sub-classes that do not have a standard idtable as state,
; but rely on a special record as state?
;
; Possible alternative approach:
;
; tok : now.Token = {
;     __blob__ : blob = <...scheme object...>
;     __dot__  : {
;       __call__ : callable = <...scheme function...>
;       __set__  : callable = <...scheme function...>
;     }
; }
;
; 'dot' will create necessary fields on the fly, depending on the kind.

;
; TODO: change file name to now-object.scm
; TODO: change table entries to have three fields:
;
; To address diamond-problem:
; * Separate interfaces and implementation.
; * Provide 'reasonable defaults' when non-ambiguous,
;   but do not when there is ambiguity.
; * Diamond problem: 'interface' will be automatically
;   populated with non-ambiguous methods. Ambiguous methods
;   will be left dangling. Attempting to call them will produce
;   an error (almost certainly at compile time)
;      -> solve it by distinguishing 'unit' and 'integration'?
; * When 'compiling to C++', interfaces for objects which are not
;   compatible according to C++ rules could be implemented as shims.
; * Maybe one could borrow ideas from the TLM here, and 'connect' interfaces,
;   so that they're just function pointers stored in an array. Multiple levels
;   of indirection, if purely static, will just return the pointer to the object,
;   not a whole chain of pointers.
;
; class foobar {
;  interface {
;
;      inherit Foo except {
;          diamond-method
;      } as foo-if
;
;      inherit Bar except {
;          diamond-method
;      } as bar-if
;      
;     take shared-method from bar
;  }
;
;  implementation {
;      state foo [Foo] implements foo-if
;      state bar [Bar] implements bar-if
;
;      method do-a-thing
; }
;
;
; Shorten the name to 'nobj' or just 'obj'
; when used as part of function and variable
; names?
;
;
; Is it possible not to adhere to a hierarchical object-oriented
; system a la C++, but provide the primitives to build one?
;
; ALTERNATELY:
;
; We distinguish between PRIMITIVE TYPES (no inheritance between them)
; and OBJECT HIERARCHY.
;
; An 'int' is not an object.
; An Int /is/ an object. It is implemented as an Object containing an int and extra stuff.
;
; study in detail: how did Python's Classic Classes work, and how do they differ from
; new-style classes in this regard?
;

;
; TODO: unify naming conventions. use now.Object instead of now.object?
; also replace 'now-object' in function name prefixes with now.Object
; as part of the unification.
;

;;
;; experimental/scratch code start
;;


;
; For ALL now-objects (not just those deriving from GenericObject)
;
; TODO: should this REPLACE the drawer/valbox structure?
;
; Current:
;
; { namespace : "name" <drawer> | "name" <drawer> | "name" <drawer> }
; { drawer    : type   <type>   | getter <getter> | setter <setter> }
; { type      : ... (driver functions) }
;
; Maybe:
; { namespace : "name" <type> | "name" <type> | "name" <type> }
; { type      : data   <data> | ... (driver functions including get/set) ...  }
; ^^ Question about this approach: how about const qualifier?
;

;;
;; experimental/scratch code end
;;

;;
;; Convenience function
;;
(define (make-default-meta) (make-fresh-drawer-table))

;;
;; shall we change 'meta' to a specific record, or leave it extensible
;; for the purposes of defining an arbitrary later-language?
;; Will now-objects be distinct from later-objects?
;; More importantly: will now-meta be distinct from later-meta?
;;

;;
;; Class creation
;;
;
; TODO/FIXME: as placeholder/experimental code, we will just check
; type-id, not the class's config parameters. This is not guarranteed
; to be correct in the long term, it is in place just to get to
; try out the code as soon as possible.
;
; Furthermore: for the time being we do not pay too much attention
; to the exact structuring of method and data members, we just want
; 'something working quickly without changing too many things at
; once'. It is something that will definitely need to be addressed
; later, especially when defining the details for the various
; later-languages.

(define (make-now-class printable-name parents) ; For class attributes, class methods, and non-virtual methods.
  (define vector-of-parents (list->vector parents))
  (define (stringify-value _)
      (string-append "<" printable-name " placeholder>")) ;; TODO: IMPROVE (recursive def?)
  (define (default-data-constructor class) (error "Default data constructor for classes not implemented yet.")) ;; # TODO: implement
  ; TODO/FIXME: contents needs to be an igel-value!
  (define state
    (make-idtable-simple-from-data #t
      `((const "parents"
               ,(now.array-ptype now.type (vector-length vector-of-parents))
               ,vector-of-parents))))
                     
  ;;
  ;; main body of make-now-class
  ;;
  (make-type-private
    (gen-type-id)
    printable-name
    parents 
    type-no-config-check ;; TODO: placeholder, might need to be changed. Think about it.
    stringify-value
    default-data-constructor
    state))

(define now.Object
  (make-now-class
    "now.Object" ; base for all objects. Maybe AnyObject?
    type-default-fallback-types
)) ;; TODO: state/constructors will be handled later.

;; Objects which inherit only from the base class shared by all objects
(define default-parents
  (list now.Object))

;
; TODO: should we get rid of this field comepletely,
; or do we keep it as an API abstraction in case we
; change the mechanics of the type system?
; We will probably keep it, yes.
;
(define (make-now-object class idtable)
  (assert (type? class))  ;; real scheme assert, to catch old usages (for now).
                          ;; later, consider whether or not we need now.Exception.
  (make-igel-value class idtable))

;
; If we specify only 'now-object', we mean now-object-value.
; TODO: fix naming convention
;
(define (now-object? value)
  (type-accepts-value? now.Object value))

(define (now-object-type? type)
  (type-accepts-type? now.Object type))

(define (now-object-lock value)
  (let* ((idtable (igel-value-get-data value))
         (is-locked (idtable-get-is-locked idtable)))
    (assert (not is-locked)) ; TODO: raise exception? Delegate responsibility for check?
    (idtable-set-is-locked-private! idtable #t)))

;
; Access functions
;

(define (now-object-try-getting-drawer now-obj key)
  (assert (now-object? now-obj)) ; TODO: now.Exception
  (let ((dtable (igel-value-get-data now-obj)))
    (drawer-table-try-getting-drawer dtable key))) ;; #f if not found

(define (now-object-try-getting-value now-obj key)
  (assert (now-object? now-obj)) ; TODO: now.Exception
  (let ((dtable (igel-value-get-data now-obj)))
    (drawer-table-try-getting-value dtable key))) ;; #f if not found

(define (now-object-get-value now-obj key)
  (let ((maybe-result (now-object-try-getting-value now-obj key)))
    (if maybe-result
      maybe-result
      ; TODO: replace with exception:
      (error (string-append "Could not access object member '" key "'")))))

(define (now-object-has-rawattr? now-obj key)
  (any->boolean (now-object-try-getting-drawer now-obj key)))

(define (now-object-get-drawer now-obj name)
  (assert (now-object? now-obj)) ;; TODO: now.Exception, etc.
  (let ((result (idtable-get-drawer (igel-value-get-data now-obj) name)))
    (assert (drawer? result)) ;; self-checking for debugging purposes, actual assertion.
    result))

; State of data structures should not be undefined in case of failures.
(define (now-object-try-setting! now-obj key value)
  ; According to current concept, is-locked does not control this.
  (assert (now-object? now-obj)) ; TODO: now.Exception
  (drawer-table-try-setting! (igel-value-get-data now-obj) key value))

(define (now-object-set! now-obj key value)
  (if (now-object-try-setting! now-obj key value)
    value
    (error (string-append "Failed to set key '" key "'")))) ; TODO: now.Exception
