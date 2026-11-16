;;;; examples/showcase.lisp
;;;; Six showcase programs highlighting Goose's strengths via cl-goose-generator.
;;;; Load in Emacs/SLIME and evaluate (generate-showcase "/tmp/goose-showcase/")
;;;; or inspect individual forms with (emit-goose *ex1-inline-orders*).

(defpackage #:cl-goose-showcase
  (:use #:cl #:cl-goose-generator)
  (:export #:generate-showcase))

(in-package #:cl-goose-showcase)

(named-readtables:in-readtable :cl-goose-generator)

(defparameter *ex1-inline-orders*
  `(do0
     (import std)
     (defstruct Item
       (sku (arr u8 varint))
       (qty varint)
       (cents varint))
     (defstruct Order
       (id varint)
       (customer (arr u8 varint))
       (items (arr Item varint)))

     (defun parse_items (field)
       (declare (type (slice u8) field)
                (values (grow Item)))
       (var ((items (list)))
         (with-block (each_split field #\;)
           (let ((p (split it #\:))
                 ((tuple qty ok1) (parse_int (aref p 1)))
                 ((tuple cents ok2) (parse_int (aref p 2))))
             (assert (and ok1 ok2))
             (dot items (push (make Item :sku (aref p 0)
                                         :qty qty
                                         :cents cents)))))
         (return items)))

     (defun main ()
       (var ((book (list) (grow Order)))
         (dot book (push (make Order :id 1001
                                     :customer "alice"
                                     :items (parse_items "SKU-441:2:1999;SKU-7:1:500"))))
         (for (o book)
           (var ((total 0))
             (for (it (dot o items))
               (incf total (* (dot it qty) (dot it cents))))
             (print (dot o customer) " owes " total " cents over "
                    (dot o items len) " items, "
                    (dot (bytes_of book) len) " bytes")))))))

(defparameter *ex2-relative-bst*
  `(do0
     (defstruct Node
       (word (cap-arr u8 8))
       (count i32 :default 1)
       (left (opt (rel Node u32)))
       (right (opt (rel Node u32))))

     (defun insert (pool word)
       (declare (type (ref (grow Node)) pool)
                (type (slice u8) word))
       (when (== (dot pool len) 0)
         (dot pool (push (make Node :word word :count 1)))
         (return))
       (var (((.= cur) (aref pool 0)))
         (loop
           (let ((c (compare word (subseq (dot cur word)))))
             (when (== c 0)
               (incf (dot cur count) 1)
               (return))
             (let ((next (if (< c 0) (dot cur left) (dot cur right))))
               (when next
                 (.= cur next)
                 (continue))
               (if (< c 0)
                   (.= (dot cur left) (dot pool (push (make Node :word word :count 1))))
                   (.= (dot cur right) (dot pool (push (make Node :word word :count 1)))))
               (return))))))

     (defun main ()
       (var ((index (list) (grow Node)))
         (with-block (each_word "the quick fox and the fox")
           (insert index it))
         (var ((image (dot index (to_bytes)) (grow u8))
               ((tuple loaded ok) ((inst from_bytes (grow Node)) image)))
           (print ok " " (dot loaded len) " nodes"))))))

(defparameter *ex3-slot-pool-sentinel*
  `(do0
     (import slot_pool)
     (defstruct Node
       (val i64)
       (prev (rel Node u32 :in (dot pool items)))
       (next (rel Node u32 :in (dot pool items))))

     (defparameter pool (make (inst slot_pool Node)))
     (defconstant (.= head)
       (with-block (dot pool (alloc_with))
         (make Node :val 0 :prev self :next self)))

     (defun insert_after (p v)
       (declare (type (ref Node) p)
                (type i64 v)
                (values (ref Node)))
       (let (((.= n) (dot pool (alloc_ref (make Node :val v :prev p :next (dot p next))))))
         (.= (dot n next prev) n)
         (.= (dot p next) n)
         (return n)))

     (defun remove (n)
       (declare (type (ref Node) n))
       (.= (dot n prev next) (dot n next))
       (.= (dot n next prev) (dot n prev))
       (dot pool (free (dot pool items (index_of n)))))

     (defun is_end (n)
       (declare (type (ref Node) n)
                (values bool))
       (.== n head))))

(defparameter *ex4-enums-and-reflection*
  `(do0
     (defenum Shape
       (Circle (r f64))
       (Rect (w f64) (h f64))
       Dot)

     (defun show (value)
       (declare (generics elem)
                (type elem value))
       (match value
         (const-for ((tuple vname vtag) (variants elem))
           ((vtag v)
            (print vname)
            (const-for ((tuple fname facc) (fields vtag))
              (print "  " fname " = " (dot v facc)))))))

     (defun reset_defaults (src)
       (declare (generics elem)
                (type (ref elem) src)
                (values elem))
       (return (make elem
                 (const-for ((tuple fname facc) (fields elem))
                   (const-if (not (has_default facc))
                     (:field facc (dot src facc))))
                 :..)))

     (defun main ()
       (var ((packed (list) (grow (var-mode Shape))))
         ,@(loop for (tag args) in '((Circle (:r 2.0))
                                     (Rect (:w 3.0 :h 4.0))
                                     (Dot ()))
                 collect `(dot packed (push (variant Shape ,tag ,@args))))
         (for (s packed)
           (match s
             ((Rect &r) (incf (dot r w) 1.0))
             (_ (scope))))))))

(defparameter *ex5-return-from-guard-defer*
  `(do0
     (import std)
     (defstruct User
       (name (cap-arr u8 16))
       (age i32))

     (defun load (text)
       (declare (type (slice u8) text)
                (values (grow User) (arr u8)))
       (var ((users (list) (grow User))
             (lineno 0))
         (with-block (each_split text #\Newline)
           (incf lineno)
           (when (> (dot it len) 0)
             (dot users (push (parse_record it lineno)))))
         (return (values users ""))))

     (defun parse_record (line lineno)
       (declare (type (slice u8) line)
                (type i64 lineno)
                (values User))
       (let ((comma (find line ",")))
         (guard (>= comma 0)
           :else (return-from load
                   (values (list) (str "line " lineno ": expected name,age"))))
         (let (((tuple age ok) (parse_int (trim (subseq line (+ comma 1))))))
           (guard ok
             :else (return-from load
                     (values (list) (str "line " lineno ": bad age"))))
           (return (make User :name (trim (subseq line 0 comma))
                              :age (as age i32))))))

     (defun with_guard_and_defer (ok)
       (declare (type bool ok))
       (guard ok :else (return))
       (defer (print "cleanup ran on every exit"))
       (print "running"))))

(defparameter *ex6-index-checkfree-threads*
  `(do0
     (defparameter nodes (list) (grow i32))
     (deftype NodeId (in u32 nodes))

     (defun sum_at (ids weights)
       (declare (checkfree)
                (type (slice NodeId) ids)
                (type (ref (grow i32)) weights)
                (values i64))
       (assert (>= (dot weights len) (dot nodes len)))
       (var ((total 0))
         (for (id ids)
           (incf total (+ (aref nodes id) (aref weights id))))
         (return total)))

     ,@(loop for (name fields) in '((Job ((n i32)))
                                    (Result ((n i32) (digits i32))))
             collect `(defstruct ,name ,@fields))

     (defun worker ()
       (declare (thread-fn))
       (loop
         (let ((job ((inst qget Job))))
           (guard (>= (dot job n) 0) :else (break))
           (qput (make Result :n (dot job n)
                              :digits (count_digits (dot job n)))))))))

(defun generate-showcase (&optional (dir "/tmp/goose-showcase/"))
  (dolist (entry `(("01_inline_orders.goose" ,*ex1-inline-orders*)
                   ("02_relative_bst.goose" ,*ex2-relative-bst*)
                   ("03_slot_pool_sentinel.goose" ,*ex3-slot-pool-sentinel*)
                   ("04_enums_reflection.goose" ,*ex4-enums-and-reflection*)
                   ("05_return_from_guard.goose" ,*ex5-return-from-guard-defer*)
                   ("06_checkfree_threads.goose" ,*ex6-index-checkfree-threads*)))
    (write-source (merge-pathnames (first entry) dir) (second entry))))
