;;;; cl-goose-generator.asd

(defsystem "cl-goose-generator"
  :description "Common Lisp DSL and code generator targeting the Goose systems language (cl-cpp-generator2 style)."
  :author "cl-goose-generator contributors"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("named-readtables" "uiop")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "symbols")
     (:file "types")
     (:file "emitter")
     (:file "writer"))))
  :in-order-to ((test-op (test-op "cl-goose-generator/tests"))))

(defsystem "cl-goose-generator/tests"
  :description "Unit, regression, and differential paren-fuzzer tests for cl-goose-generator."
  :depends-on ("cl-goose-generator" "named-readtables" "uiop")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "suite")
     (:file "fuzzer"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-goose-generator/tests '#:run-all-tests)))
