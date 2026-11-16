;;;; src/package.lisp

(defpackage #:cl-goose-generator
  (:nicknames #:goose #:cl-goose)
  (:use #:cl)
  (:export
   ;; Readtable & emission entry points
   #:*omit-redundant-parentheses*
   #:*file-hashes*
   #:*indent-width*
   #:*auto-format*
   #:emit-goose
   #:emit-type
   #:write-source
   #:goose-symbol-name
   ;; Top-level & declarations
   #:do0
   #:scope
   #:block
   #:import
   #:namespace
   #:namespace-alias
   #:ns
   #:deftype
   #:defstruct
   #:defroot
   #:defenum
   #:defparameter
   #:defconstant
   #:defconst
   #:reusable-var
   #:reusable-slice-var
   #:extern-lib
   #:defun
   #:declare
   #:values
   #:generics
   #:checkfree
   #:simd
   #:inline
   #:recursive
   #:thread-fn
   #:export
   #:extern
   ;; Bindings & assignments
   #:let
   #:let*
   #:var
   #:const
   #:tuple
   #:.=
   #:setf
   #:incf
   #:decf
   #:+= #:-= #:*= #:/= #:%=
   #:&= #:\|= #:^= #:<<= #:>>=
   ;; Types
   #:arr
   #:cap-arr
   #:grow
   #:stack
   #:slice
   #:ref
   #:opt
   #:rel
   #:in
   #:upto
   #:var-mode
   #:fn-type
   #:inst
   #:pad
   #:varint
   #:i8 #:i16 #:i32 #:i64
   #:u8 #:u16 #:u32 #:u64
   #:f32 #:f64
   #:bool
   ;; Expressions & operators
   #:dot
   #:aref
   #:subseq
   #:^
   #:make
   #:variant
   #:with-block
   #:=>
   #:list
   #:fill
   #:cap
   #:range
   #:as
   #:as!
   #:addr
   #:&
   #:&& #:\|\| #:!
   #:\|
   #:== #:!= #:.== #:.!=
   #:<< #:>>
   #:~
   #:raw-string
   #:raw
   #:comments
   #:block-comment
   #:space
   ;; Control flow & Goose specials
   #:if
   #:when
   #:unless
   #:cond
   #:match
   #:guard
   #:defer
   #:loop
   #:while
   #:for
   #:dotimes
   #:dolist
   #:break
   #:continue
   #:return
   #:return-from
   #:const-for
   #:const-if
   #:const-cond
   #:fields
   #:variants
   #:types
   #:has_default
   #:same
   #:self
   #:null
   #:true
   #:false))

(in-package #:cl-goose-generator)

;; File-local readtable for mixed-case Goose identifiers (Node, Shape, etc.)
;; without ever mutating global *readtable*.
(named-readtables:defreadtable :cl-goose-generator
  (:merge :standard)
  (:case :invert))
