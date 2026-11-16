;;;; tests/suite.lisp
;;;; FiveAM test suite for cl-goose-generator, including readtable hygiene,
;;;; foreign-package dispatch, write-source cache, arity checks, and full Goose spec.

(defpackage #:cl-goose-generator/tests
  (:use #:cl #:fiveam)
  (:import-from #:cl-goose-generator
                #:emit-goose
                #:emit-type
                #:write-source
                #:goose-symbol-name
                #:*omit-redundant-parentheses*
                #:*file-hashes*)
  (:export #:run-all-tests))

(in-package #:cl-goose-generator/tests)

(named-readtables:in-readtable :cl-goose-generator)

(def-suite :cl-goose-generator)
(in-suite :cl-goose-generator)

(defun trim-ws (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return) s))

;;; ---------------------------------------------------------------------------
;;; 1. Readtable hygiene & foreign package safety (cl-py-generator review traps)
;;; ---------------------------------------------------------------------------

(test readtable-not-mutated-globally
  (let ((std-rt (copy-readtable nil)))
    (is (eq :upcase (readtable-case std-rt)))))

(test symbol-name-normalization-preserves-strings
  ;; Strings must NEVER be lowercased
  (is (string= "API" (goose-symbol-name "API")))
  (is (string= "SIZE" (goose-symbol-name "SIZE")))
  ;; All-uppercase symbols (e.g. read under :upcase) become lowercase
  (is (string= "foo" (goose-symbol-name (intern "FOO" :cl-goose-generator/tests))))
  ;; Mixed-case symbols (e.g. under :invert or |Point|) keep exact case
  (is (string= "Point" (goose-symbol-name 'Point)))
  (is (string= "Shape" (goose-symbol-name 'Shape))))

(test upcase-readtable-forms-work
  ;; Simulate reading forms under standard :upcase readtable
  (let ((upcase-rt (copy-readtable nil)))
    (let ((*readtable* upcase-rt))
      (let ((form (read-from-string "(MAKE |Point| :X 1 :Y 2)")))
        (is (string= "Point { x: 1, y: 2 }" (emit-goose form)))))))

(test foreign-package-dispatch
  ;; Forms interned in a completely unrelated package must still dispatch cleanly
  (let ((pkg (or (find-package :cl-goose-foreign-test)
                 (make-package :cl-goose-foreign-test :use nil))))
    (unwind-protect
         (let ((form (let ((*package* pkg))
                       (read-from-string
                        "(DO0 (DEFUN MAIN () (LET ((X 1)) (RETURN X))))"))))
           (is (search "fn main() {" (emit-goose form)))
           (is (search "let x = 1;" (emit-goose form)))
           (is (search "return x;" (emit-goose form))))
      (delete-package pkg))))

;;; ---------------------------------------------------------------------------
;;; 2. Types (full Goose §2, §5, §9, §10, §11, §15)
;;; ---------------------------------------------------------------------------

(test all-goose-types
  (is (string= "i64" (emit-type 'i64)))
  (is (string= "u8[4]" (emit-type '(arr u8 4))))
  (is (string= "u8[]" (emit-type '(arr u8))))
  (is (string= "u8[varint]" (emit-type '(arr u8 varint))))
  (is (string= "u8[..16]" (emit-type '(cap-arr u8 16))))
  (is (string= "i64[..]" (emit-type '(cap-arr i64))))
  (is (string= "Node[>..]" (emit-type '(grow Node))))
  (is (string= "i64[>..<]" (emit-type '(stack i64))))
  (is (string= "const u8[:]" (emit-type '(const (slice u8)))))
  (is (string= "const (u8[:])&" (emit-type '(const (ref (slice u8))))))
  (is (string= "Node?" (emit-type '(opt Node))))
  (is (string= "Node&<u32>?" (emit-type '(opt (rel Node u32)))))
  (is (string= "Node&<u32 in pool.items>" (emit-type '(rel Node u32 :in (dot pool items)))))
  (is (string= "(u32 in nodes)[>..]" (emit-type '(grow (in u32 nodes)))))
  (is (string= "(Mark..&<u32 in marks>)[>..]"
               (emit-type '(grow (rel (var-mode Mark) u32 :in marks)))))
  (is (string= "u32 upto edges" (emit-type '(upto u32 edges))))
  (is (string= "Shape.Circle" (emit-type '(variant-type Shape Circle))))
  (is (string= "fn(i64, f64) -> bool" (emit-type '(fn-type (i64 f64) bool)))))

;;; ---------------------------------------------------------------------------
;;; 3. Operator arity, precedence, Oracle mode & dot-on-number
;;; ---------------------------------------------------------------------------

(test operator-arity-checks
  (signals error (emit-goose '(+)))
  (signals error (emit-goose '(-)))
  (signals error (emit-goose '(*)))
  (signals error (emit-goose '(/ 1)))
  (signals error (emit-goose '(% 1)))
  (signals error (emit-goose '(== 1)))
  (signals error (emit-goose '(< 1)))
  (signals error (emit-goose '(<< 1)))
  (signals error (emit-goose '(and)))
  (signals error (emit-goose '(or)))
  (signals error (emit-goose '(not a b)))
  ;; Single-arg unary + / * / and / or stay valid for CL macros
  (is (string= "x" (emit-goose '(+ x))))
  (is (string= "255" (emit-goose '(or 255))))
  (is (string= "-x" (emit-goose '(- x))))
  (is (string= "-(-3)" (emit-goose '(- -3)))))

(test precedence-and-oracle-mode
  (is (string= "a + b * c" (emit-goose '(+ a (* b c)))))
  (is (string= "(a + b) * c" (emit-goose '(* (+ a b) c))))
  (is (string= "((a + b) * c)"
               (emit-goose '(* (+ a b) c) :omit-redundant-parentheses nil)))
  ;; Non-chaining comparisons
  (is (string= "(a == b) == c" (emit-goose '(== (== a b) c))))
  ;; Numbers as dot/aref receivers are parenthesized in both modes
  (is (string= "(3).abs()" (emit-goose '(dot 3 (abs)))))
  (is (string= "(-3).abs()" (emit-goose '(dot -3 (abs)) :omit-redundant-parentheses nil)))
  ;; Block-ended expression inside binary op is parenthesized
  (is (string= "(if a > 2 { 1 } else { 2 }) + 3"
               (emit-goose '(+ (if (> a 2) 1 2) 3)))))

;;; ---------------------------------------------------------------------------
;;; 4. Core Goose constructs (structs, roots, enums, functions, control flow)
;;; ---------------------------------------------------------------------------

(test structs-roots-and-enums
  (let ((s (emit-goose
            '(defstruct Hdr
               (magic (arr u8 4))
               (pad)
               (size u32)
               (kind u8 :let t)
               (name (cap-arr u8 8) :const t)))))
    (is (search "struct Hdr" s))
    (is (search "pad," s))
    (is (search "let kind: u8" s))
    (is (search "const name: u8[..8]" s)))

  (let ((r (emit-goose
            '(defroot (Workspace T)
               (items (grow T) :default (list))
               (calls i64 :default 0)))))
    (is (search "root struct Workspace<T>" r))
    (is (search "items: T[>..] = []" r)))

  (let ((e (emit-goose
            '(defenum Shape
               (Circle (r f64))
               (Rect (w f64) (h f64))
               Dot))))
    (is (string= "enum Shape { Circle { r: f64 }, Rect { w: f64, h: f64 }, Dot }"
                 (trim-ws e)))))

(test defun-declare-qualifiers-and-values
  (let ((f1 (emit-goose
             '(defun sum_at (ids weights)
                (declare (checkfree)
                         (type (slice NodeId) ids)
                         (type (ref (grow i32)) weights)
                         (values i64))
                (return 0)))))
    (is (search "checkfree fn sum_at(ids: NodeId[:], weights: i32[>..]&) -> i64" f1)
        "Expected checkfree signature"))

  (let ((f2 (emit-goose
             '(defun add (a b)
                (declare (export "c_add")
                         (type i32 a b)
                         (values i32))
                (+ a b)))))
    (is (search "export \"c_add\" fn add(a: i32, b: i32) -> i32 {" f2))
    (is (search "a + b" f2)))

  (let ((f3 (emit-goose
             '(defun limit (x &optional (lo 0) (hi 100))
                (declare (type i64 x lo hi) (values i64))))))
    (is (search "fn limit(x: i64, lo: i64 = 0, hi: i64 = 100) -> i64 {}" f3))))

(test bindings-tuple-ref-and-loops
  (let ((code (emit-goose
               '(defun demo (pool xs)
                  (declare (type (ref (grow Node)) pool)
                           (type (grow i64) xs))
                  (var ((items (list))
                        ((.= cur) (aref pool 0))
                        ((tuple q ok) (divide 7 2)))
                    (for (&x xs)
                      (incf x))
                    (for ((tuple v i) xs)
                      (incf v i))
                    (for ((tuple &w j) xs)
                      (incf w j))
                    (.= cur (aref pool 1)))))))
    (is (search "var items = [];" code))
    (is (search "var cur .= pool[0];" code))
    (is (search "var q, ok = divide(7, 2);" code))
    (is (search "for &x in xs {" code))
    (is (search "x++;" code))
    (is (search "for v, i in xs {" code))
    (is (search "v += i;" code))
    (is (search "for &w, j in xs {" code))
    (is (search "cur .= pool[1];" code))))

(test const-for-and-const-if-reflection
  ;; Under :invert readtable, uppercase N/A/T invert to lowercase n/a/t;
  ;; strings ("T") preserve uppercase verbatim.
  (let ((code (emit-goose
               '(defun reset_defaults (src)
                  (declare (generics T)
                           (type (ref T) src)
                           (values T))
                  (return (make T
                            (const-for ((tuple N A) (fields T))
                              (const-if (not (has_default A))
                                (:field A (dot src A))))
                            :..))))))
    (is (search "const for <n, a> in fields<t>" code))
    (is (search "const if !has_default<a>" code))
    (is (search "a: src.a," code))
    (is (search ".. }" code)))
  (let ((code-upper (emit-goose
                     '(defun reset_defaults (src)
                        (declare (generics "T")
                                 (type (ref "T") src)
                                 (values "T"))
                        (return (make "T"
                                  (const-for ((tuple "N" "A") (fields "T"))
                                    (const-if (not (has_default "A"))
                                      (:field "A" (dot src "A"))))
                                  :..))))))
    (is (search "const for <N, A> in fields<T>" code-upper))
    (is (search "const if !has_default<A>" code-upper))
    (is (search "A: src.A," code-upper))))

(test guard-defer-match-and-return-from
  (let ((code (emit-goose
               '(defun parse_record (line lineno)
                  (declare (type (slice u8) line)
                           (type i64 lineno)
                           (values User))
                  (guard (>= lineno 0)
                    :else (return-from load
                            (values (list) (str "bad line"))))
                  (defer (print "done"))
                  (match lineno
                    ((or 1 (range 2 5)) "low")
                    (_ "high"))))))
    (is (search "guard lineno >= 0 else {" code))
    (is (search "return [], str(\"bad line\") from load;" code))
    (is (search "defer print(\"done\");" code))
    (is (search "1, 2..5 => \"low\"," code))))

(test comments-multiline-without-regex
  (is (string= (format nil "// line1~%// line2~%")
               (emit-goose `(comments ,(format nil "line1~%line2"))))))

;;; ---------------------------------------------------------------------------
;;; 5. write-source cache safety (deleted file rewrite & post-write hash)
;;; ---------------------------------------------------------------------------

(test write-source-cache-and-deleted-file
  (let ((*file-hashes* (make-hash-table :test #'equal)))
    (uiop:with-temporary-file (:pathname tmp :type "goose" :keep nil)
      (delete-file tmp)
      (let ((prog '(defun main () (print "v1"))))
        ;; First write creates the file
        (multiple-value-bind (p1 w1) (write-source tmp prog)
          (is (probe-file p1))
          (is-true w1))
        ;; Second write with identical content skips disk write
        (multiple-value-bind (p2 w2) (write-source tmp prog)
          (declare (ignore p2))
          (is-false w2))
        ;; Deleting file on disk MUST force a rewrite even though cache matches
        (delete-file tmp)
        (multiple-value-bind (p3 w3) (write-source tmp prog)
          (is (probe-file p3))
          (is-true w3))))))

(defun run-all-tests ()
  (run! :cl-goose-generator))
