# cl-goose-generator

A Common Lisp ASDF code generator (`cl-cpp-generator2` style) that emits idiomatic [Goose](https://github.com/nicolae-b/goose) source code from S-expressions, designed for interactive Emacs + SLIME workflows.

## Installation (Quicklisp + Emacs / SLIME)

1. Place or symlink `cl-goose-generator/` into `~/quicklisp/local-projects/cl-goose-generator/`.
2. Load in your SLIME REPL:

```lisp
(ql:quickload :cl-goose-generator)
```

3. Run the FiveAM + differential Paren Fuzzer test suite:

```lisp
(asdf:test-system :cl-goose-generator)
```

## File-Local Readtable Hygiene

Loading `:cl-goose-generator` **never** mutates your global `*readtable*`.
In files that use mixed-case Goose type names (`Node`, `Shape`, `Unit`), opt into the package's inverted readtable locally:

```lisp
(defpackage #:my-goose-app
  (:use #:cl #:cl-goose-generator))

(in-package #:my-goose-app)
(named-readtables:in-readtable :cl-goose-generator)
```

- Symbols with all-lowercase input (like `foo` or `:kind`) emit as lowercase (`foo`, `kind`).
- Mixed-case symbols (`Node`, `Shape`) preserve exact case.
- Strings passed as identifier names (e.g. `"T"`, `"SIZE"`, `"API"`) are always preserved verbatim by `goose-symbol-name`.
- Calling `emit-goose` or `write-source` from any package works cleanly without `:use :cl-goose-generator` because form dispatch is package-agnostic.

## Quick Example

```lisp
(write-source #P"hello.goose"
  `(do0
     (import std)
     (defstruct Node
       (val i64)
       (next (opt (rel Node u32))))
     (defun main ()
       (var ((pool (list) (grow Node)))
         ,@(loop for v in '(10 20 30)
                 collect `(dot pool (push (make Node :val ,v))))
         (for ((tuple &item i) pool)
           (print i ": " (dot item val)))))))
```
