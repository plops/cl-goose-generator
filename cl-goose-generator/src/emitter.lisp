;;;; src/emitter.lisp
;;;; S-expression to Goose source emitter (cl-cpp-generator2 style).

(in-package #:cl-goose-generator)

(defvar *omit-redundant-parentheses* t
  "When T, omit parentheses based on Goose operator precedence.
When NIL (Oracle mode), parenthesize all compound subexpressions explicitly.")

(defvar *indent-width* 4
  "Spaces per indentation level.")

(defvar *make-context-p* nil
  "T when emitting struct literal initializers (for const-for/const-if).")
(defvar *match-context-p* nil
  "T when emitting match arms (for const-for/const-if).")
(defvar *const-cond-expr-p* nil
  "T when emitting a const-if condition (renders has_default<A>, same<T1, T2>).")

(defun indent-str (&optional (level 1))
  (make-string (* level *indent-width*) :initial-element #\Space))

(defun indent-lines (text &optional (level 1))
  "Prefix every non-empty line in TEXT with indentation LEVEL."
  (if (zerop level)
      text
      (let ((pad (indent-str level))
            (len (length text)))
    (if (zerop len)
        ""
        (with-output-to-string (out)
          (loop with start = 0
                for pos = (position #\Newline text :start start)
                for line = (subseq text start (or pos len))
                do (unless (zerop (length line))
                     (write-string pad out)
                     (write-string line out))
                when pos do (write-char #\Newline out)
                while pos
                do (setf start (1+ pos))))))))

;;; ---------------------------------------------------------------------------
;;; Operator table & precedence (Goose Reference Appendix A)
;;; ---------------------------------------------------------------------------

(defun infix-operator-spec (head)
  "Return (values goose-op precedence min-args max-args non-assoc-p) or NIL."
  (cond
    ((or (string= head "OR") (string= head "||"))
     (values "||" 1 1 nil nil))
    ((or (string= head "AND") (string= head "&&"))
     (values "&&" 2 1 nil nil))
    ((string= head "==")  (values "==" 3 2 2 t))
    ((string= head "!=")  (values "!=" 3 2 2 t))
    ((string= head ".==") (values ".==" 3 2 2 t))
    ((string= head ".!=") (values ".!=" 3 2 2 t))
    ((string= head "<")   (values "<" 4 2 2 t))
    ((string= head "<=")  (values "<=" 4 2 2 t))
    ((string= head ">")   (values ">" 4 2 2 t))
    ((string= head ">=")  (values ">=" 4 2 2 t))
    ((or (string= head "LOGIOR") (string= head "|"))
     (values "|" 5 1 nil nil))
    ((or (string= head "LOGXOR") (string= head "^"))
     (values "^" 6 1 nil nil))
    ((or (string= head "LOGAND") (string= head "&"))
     (values "&" 7 1 nil nil))
    ((string= head "<<")  (values "<<" 8 2 2 nil))
    ((string= head ">>")  (values ">>" 8 2 2 nil))
    ((string= head "+")   (values "+" 9 1 nil nil))
    ((string= head "-")   (values "-" 9 1 nil nil))
    ((string= head "*")   (values "*" 10 1 nil nil))
    ((string= head "/")   (values "/" 10 2 2 nil))
    ((or (string= head "%") (string= head "MOD"))
     (values "%" 10 2 2 nil))
    (t nil)))

(defun check-operator-arity (form min-args max-args)
  (let ((n (length (cdr form))))
    (when (< n min-args)
      (error "Operator ~s requires at least ~d argument~:p, got ~d in ~s"
             (car form) min-args n form))
    (when (and max-args (> n max-args))
      (error "Operator ~s accepts at most ~d argument~:p, got ~d in ~s"
             (car form) max-args n form))))

(defun block-ended-expr-p (form)
  "Return T when FORM compiles to a Goose block-ended construct (if, match, block, scope)."
  (and (consp form)
       (symbolp (car form))
       (sym-member (car form) '("IF" "COND" "MATCH" "BLOCK" "SCOPE"))))

(defun statement-only-form-p (form)
  "Return T if FORM is strictly a statement (never a trailing value expression)."
  (and (consp form)
       (or (and (symbolp (car form))
                (sym-member (car form)
                            '("DO0" "SETF" ".=" "INCF" "DECF"
                              "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>="
                              "LET" "LET*" "VAR" "CONST"
                              "WHEN" "UNLESS" "GUARD" "DEFER"
                              "LOOP" "WHILE" "FOR" "DOTIMES" "DOLIST"
                              "BREAK" "CONTINUE" "RETURN" "RETURN-FROM"
                              "COMMENTS" "BLOCK-COMMENT" "SPACE" "RAW"
                              "CONST-FOR" "CONST-IF" "CONST-COND"
                              "PRINT" "FORMAT" "ASSERT" "ABORT" "EXIT"
                              "QPUT" "QLIMIT")))
           ;; Void mutating dot-calls like xs.push(...), xs.clear(), pool.free(...)
           (and (sym-eq (car form) "DOT")
                (let ((last-part (car (last (cdr form)))))
                  (and (consp last-part)
                       (sym-member (car last-part)
                                   '("APPEND" "CLEAR" "RESIZE" "POP_N"
                                     "FREE" "FREE_SLICE" "MOVE_FROM"))))))))

(defun negative-number-p (form)
  (and (numberp form) (< form 0)))

(defun wrap-parens (str)
  (format nil "(~a)" str))

(defun emit-expr-prec (form min-prec)
  "Emit FORM as a Goose expression inside a parent context of precedence MIN-PREC."
  (multiple-value-bind (text prec) (emit-expr-with-prec form)
    (cond
      ((null *omit-redundant-parentheses*)
       ;; Oracle mode: parenthesize everything non-atomic, plus negative numbers
       ;; and any number/block receiver inside postfix context.
       (if (or (< prec 12)
               (and (numberp form) (>= min-prec 11)))
           (wrap-parens text)
           text))
      (t
       (if (or (< prec min-prec)
               (and (numberp form) (< form 0) (>= min-prec 11))
               (and (numberp form) (>= min-prec 12)))
           (wrap-parens text)
           text)))))

(defun emit-operand-for-postfix (form)
  "Emit receiver for dot/aref/subseq; safely parenthesizes numbers and operators."
  (emit-expr-prec form 12))

(defun emit-expr (form)
  "Emit FORM as a top-level Goose expression."
  (emit-expr-prec form 0))

;;; ---------------------------------------------------------------------------
;;; Core expression emitter returning (values string precedence)
;;; ---------------------------------------------------------------------------

(defun emit-float-literal (val)
  (let* ((s (let ((*read-default-float-format* 'double-float))
              (format nil "~a" val)))
         ;; Normalize SBCL exponent markers d0/f0/s0/l0 to e
         (clean (substitute #\e #\d (substitute #\e #\D
                 (substitute #\e #\f (substitute #\e #\F s))))))
    (if (or (find #\. clean) (find #\e clean))
        clean
        (concatenate 'string clean ".0"))))

(defun emit-expr-with-prec (form)
  (cond
    ((integerp form)
     (values (format nil "~d" form) 12))
    ((floatp form)
     (values (emit-float-literal form) 12))
    ((characterp form)
     (values (escape-goose-char form) 12))
    ((stringp form)
     (values (escape-goose-string form) 12))
    ((null form)
     (values "null" 12))
    ((eq form t)
     (values "true" 12))
    ((symbolp form)
     (let ((k (sym-key form)))
       (cond
         ((string= k "TRUE")  (values "true" 12))
         ((string= k "FALSE") (values "false" 12))
         ((string= k "NULL")  (values "null" 12))
         ((string= k "SELF")  (values "self" 12))
         ((string= k "_")     (values "_" 12))
         ((string= k "[]")    (values "[]" 12))
         (t (values (goose-symbol-name form) 12)))))
    ((not (consp form))
     (error "Cannot emit Goose expression from ~s" form))
    (t
     (let ((head (car form))
           (args (cdr form)))
       ;; Generic callee invocation: ((inst f T) arg1 arg2)
       (when (and (consp head) (sym-eq (car head) "INST"))
         (return-from emit-expr-with-prec
           (values (format nil "~a(~{~a~^, ~})"
                           (emit-type head)
                           (mapcar #'emit-expr args))
                   12)))
       (unless (symbolp head)
         (error "Expression head must be a symbol or (inst ...), got ~s" form))
       (let ((k (sym-key head)))
         (cond
           ;; Namespaces
           ((string= k "NS")
            (unless (= (length args) 2)
              (error "NS expects 2 arguments (ns name), got ~s" form))
            (values (if (null (first args))
                        (format nil "::~a" (goose-symbol-name (second args)))
                        (format nil "~a::~a"
                                (goose-symbol-name (first args))
                                (goose-symbol-name (second args))))
                    12))

           ;; Raw string """..."""
           ((string= k "RAW-STRING")
            (unless (= (length args) 1)
              (error "RAW-STRING expects 1 string argument, got ~s" form))
            (values (format nil "\"\"\"~a\"\"\"" (first args)) 12))

           ;; Raw Goose escape hatch
           ((string= k "RAW")
            (unless (= (length args) 1)
              (error "RAW expects 1 string argument, got ~s" form))
            (values (first args) 12))

           ;; Generic instantiation as expression/value: (inst f T)
           ((string= k "INST")
            (values (emit-type form) 12))

           ;; Array literals: (list ...), (fill v n), (cap n)
           ((string= k "LIST")
            (values (format nil "[~{~a~^, ~}]" (mapcar #'emit-expr args)) 12))

           ((string= k "FILL")
            (unless (= (length args) 2)
              (error "FILL expects (fill value count), got ~s" form))
            (values (format nil "[~a; ~a]"
                            (emit-expr (first args))
                            (emit-expr (second args)))
                    12))

           ((string= k "CAP")
            (unless (= (length args) 1)
              (error "CAP expects (cap count), got ~s" form))
            (values (format nil "[..~a]" (emit-expr (first args))) 12))

           ;; From-end offset ^k
           ((and (string= k "^") (= (length args) 1))
            (values (format nil "^~a" (emit-expr-prec (first args) 12)) 12))

           ;; Unary operators: ! / not, ~ / lognot, & / addr
           ((or (string= k "NOT") (string= k "!"))
            (check-operator-arity form 1 1)
            (values (format nil "!~a" (emit-expr-prec (first args) 11)) 11))

           ((or (string= k "LOGNOT") (string= k "~"))
            (check-operator-arity form 1 1)
            (values (format nil "~~~a" (emit-expr-prec (first args) 11)) 11))

           ((or (string= k "ADDR") (and (string= k "&") (= (length args) 1)))
            (check-operator-arity form 1 1)
            (values (format nil "&~a" (emit-expr-prec (first args) 11)) 11))

           ;; Casts: (as expr T), (as! expr T), (coerce expr T)
           ((or (string= k "AS") (string= k "COERCE"))
            (unless (= (length args) 2)
              (error "~a expects (as expr type), got ~s" k form))
            (values (format nil "~a as ~a"
                            (emit-expr-prec (first args) 12)
                            (emit-type (second args)))
                    12))

           ((string= k "AS!")
            (unless (= (length args) 2)
              (error "AS! expects (as! expr type), got ~s" form))
            (values (format nil "~a as! ~a"
                            (emit-expr-prec (first args) 12)
                            (emit-type (second args)))
                    12))

           ;; Bit shift ash
           ((string= k "ASH")
            (unless (= (length args) 2)
              (error "ASH expects 2 arguments, got ~s" form))
            (let ((count (second args)))
              (if (and (integerp count) (< count 0))
                  (values (format nil "~a >> ~d"
                                  (emit-expr-prec (first args) 8)
                                  (- count))
                          8)
                  (values (format nil "~a << ~a"
                                  (emit-expr-prec (first args) 8)
                                  (emit-expr-prec count 9))
                          8))))

           ;; Const-if reflection queries: has_default<A>, same<T1, T2>
           ((and *const-cond-expr-p* (string= k "HAS_DEFAULT"))
            (unless (= (length args) 1)
              (error "HAS_DEFAULT expects 1 accessor, got ~s" form))
            (values (format nil "has_default<~a>" (goose-symbol-name (first args))) 12))

           ((and *const-cond-expr-p* (string= k "SAME"))
            (unless (= (length args) 2)
              (error "SAME expects 2 types, got ~s" form))
            (values (format nil "same<~a, ~a>"
                            (emit-type (first args))
                            (emit-type (second args)))
                    12))

           ;; Dot member/method chains: (dot obj f1 (m a b))
           ((string= k "DOT")
            (when (null args)
              (error "DOT expects at least 1 argument, got ~s" form))
            (let ((result (emit-operand-for-postfix (first args))))
              (dolist (part (rest args))
                (cond
                  ((or (symbolp part) (stringp part))
                   (setf result (format nil "~a.~a" result (goose-symbol-name part))))
                  ((consp part)
                   (let ((mhead (car part))
                         (margs (cdr part)))
                     (if (and (consp mhead) (sym-eq (car mhead) "INST"))
                         (setf result (format nil "~a.~a(~{~a~^, ~})"
                                              result
                                              (emit-type mhead)
                                              (mapcar #'emit-expr margs)))
                         (setf result (format nil "~a.~a(~{~a~^, ~})"
                                              result
                                              (goose-symbol-name mhead)
                                              (mapcar #'emit-expr margs))))))
                  (t (error "Invalid DOT member ~s in ~s" part form))))
              (values result 12)))

           ;; Indexing: (aref arr idx)
           ((string= k "AREF")
            (unless (= (length args) 2)
              (error "AREF expects (aref arr idx), got ~s" form))
            (values (format nil "~a[~a]"
                            (emit-operand-for-postfix (first args))
                            (emit-expr (second args)))
                    12))

           ;; Slicing: (subseq arr [lo [hi]]) or (slice arr [lo [hi]])
           ((or (string= k "SUBSEQ") (string= k "SLICE"))
            (unless (<= 1 (length args) 3)
              (error "~a expects (subseq arr [lo [hi]]), got ~s" k form))
            (let ((arr (emit-operand-for-postfix (first args)))
                  (lo (second args))
                  (hi (third args)))
              (values (format nil "~a[~a..~a]"
                              arr
                              (if lo (emit-expr lo) "")
                              (if hi (emit-expr hi) ""))
                      12)))

           ;; Struct literal: (make Type ...)
           ((string= k "MAKE")
            (when (null args)
              (error "MAKE expects a type name, got ~s" form))
            (values (emit-make-literal (first args) (rest args)) 12))

           ;; Enum variant literal: (variant Enum Variant ...)
           ((string= k "VARIANT")
            (unless (>= (length args) 2)
              (error "VARIANT expects (variant Enum Variant ...), got ~s" form))
            (let ((enum-str (emit-type (first args)))
                  (var-str (goose-symbol-name (second args)))
                  (field-args (cddr args)))
              (if (null field-args)
                  (values (format nil "~a.~a" enum-str var-str) 12)
                  (values (emit-make-literal
                           (format nil "~a.~a" enum-str var-str)
                           field-args)
                          12))))

           ;; Range expression a..b
           ((string= k "RANGE")
            (unless (= (length args) 2)
              (error "RANGE expects (range lo hi), got ~s" form))
            (values (format nil "~a..~a"
                            (emit-expr (first args))
                            (emit-expr (second args)))
                    12))

           ;; Trailing block call: (with-block call [(=> a b)] body...)
           ((string= k "WITH-BLOCK")
            (values (emit-with-block form) 12))

           ;; Expression-position if / cond / match / block / scope
           ((string= k "IF")
            (values (emit-if form :expr-p t) 0))

           ((string= k "COND")
            (values (emit-cond form :expr-p t) 0))

           ((string= k "MATCH")
            (values (emit-match form :expr-p t) 0))

           ((string= k "BLOCK")
            (values (emit-block-like "block" args :expr-p t) 0))

           ((string= k "SCOPE")
            (values (emit-block-like nil args :expr-p t) 0))

           ;; Infix & arithmetic operators
           (t
            (multiple-value-bind (op prec min-args max-args non-assoc-p)
                (infix-operator-spec k)
              (if op
                  (progn
                    (check-operator-arity form min-args max-args)
                    (cond
                      ;; 1-arg + / * / and / or / log*
                      ((and (= (length args) 1) (not (string= op "-")))
                       (emit-expr-with-prec (first args)))
                      ;; 1-arg unary -
                      ((and (= (length args) 1) (string= op "-"))
                       (values (format nil "-~a" (emit-expr-prec (first args) 11)) 11))
                      ;; 2+ args binary/n-ary
                      (t
                       (let ((child-min-prec (if non-assoc-p (1+ prec) prec)))
                         (values
                          (with-output-to-string (out)
                            (write-string (emit-expr-prec (first args) child-min-prec) out)
                            (dolist (rhs (rest args))
                              (format out " ~a ~a" op
                                      (emit-expr-prec rhs (1+ prec)))))
                          prec)))))
                  ;; Regular function call
                  (values (format nil "~a(~{~a~^, ~})"
                                  (goose-symbol-name head)
                                  (mapcar #'emit-expr args))
                          12))))))))))

;;; ---------------------------------------------------------------------------
;;; Struct / Variant literals (make, variant, const-for/const-if inside make)
;;; ---------------------------------------------------------------------------

(defun emit-make-entries (items)
  "Emit struct initializer entries from ITEMS (keyword pairs, positionals, :../:defaults, const-for)."
  (let ((entries '())
        (has-expansion nil))
    (loop while items do
      (let ((cur (pop items)))
        (cond
          ((or (sym-eq cur "..") (sym-eq cur "DEFAULTS"))
           (push ".." entries))
          ((keywordp cur)
           (when (null items)
             (error "Missing value after field keyword ~s in MAKE" cur))
           (let ((val (pop items)))
             (push (format nil "~a: ~a" (goose-symbol-name cur) (emit-expr val))
                   entries)))
          ((and (consp cur) (sym-eq (car cur) "FIELD"))
           (destructuring-bind (_ field-expr val-expr) cur
             (declare (ignore _))
             (push (format nil "~a: ~a"
                           (emit-expr field-expr)
                           (emit-expr val-expr))
                   entries)))
          ((and (consp cur) (sym-member (car cur) '("CONST-FOR" "CONST-IF" "CONST-COND")))
           (setf has-expansion t)
           (let ((*make-context-p* t))
             (push (emit-stmt-raw cur) entries)))
          (t
           (push (emit-expr cur) entries)))))
    (values (nreverse entries) has-expansion)))

(defun emit-make-literal (type-spec field-args)
  (let ((type-str (if (stringp type-spec) type-spec (emit-type type-spec))))
    (if (null field-args)
        (format nil "~a {}" type-str)
        (multiple-value-bind (entries has-expansion) (emit-make-entries field-args)
          (declare (ignore has-expansion))
          (format nil "~a { ~{~a~^, ~} }" type-str entries)))))

;;; ---------------------------------------------------------------------------
;;; Trailing block calls: (with-block call [(=> a b)] body...)
;;; ---------------------------------------------------------------------------

(defun emit-with-block (form)
  (let* ((args (cdr form))
         (call-form (first args))
         (rest-forms (rest args))
         (params nil)
         (body rest-forms))
    (when (and (consp (first rest-forms))
               (sym-eq (car (first rest-forms)) "=>"))
      (setf params (mapcar #'goose-symbol-name (cdr (first rest-forms)))
            body (rest rest-forms)))
    (let ((call-str (emit-expr call-form)))
      (if (null body)
          (if params
              (format nil "~a { ~{~a~^, ~} => }" call-str params)
              (format nil "~a {}" call-str))
          (let* ((header-prefix (if params
                                    (format nil "~{~a~^, ~} =>" params)
                                    nil))
                 (single-expr (and (= (length body) 1)
                                   (not (statement-only-form-p (first body))))))
            (if (and single-expr (null header-prefix))
                (format nil "~a { ~a }" call-str (emit-expr (first body)))
                (if single-expr
                    (format nil "~a { ~a ~a }"
                            call-str header-prefix (emit-expr (first body)))
                    (let* ((inner (emit-body-forms body :expr-last-p nil :indent 1))
                           (param-line (when header-prefix
                                         (format nil "~a~a~%"
                                                 (indent-str 1)
                                                 header-prefix))))
                      (format nil "~a {~%~@[~a~]~a}"
                              call-str
                              param-line
                              inner)))))))))

;;; ---------------------------------------------------------------------------
;;; Bindings & Declarations (let, var, const, defparameter, defconstant)
;;; ---------------------------------------------------------------------------

(defun emit-binding-lhs (lhs)
  "Return (values lhs-str op-str) for a binding target: x, (.= x), or (tuple a b)."
  (cond
    ((or (symbolp lhs) (stringp lhs))
     (values (goose-symbol-name lhs) "="))
    ((and (consp lhs) (sym-eq (car lhs) ".="))
     (unless (= (length lhs) 2)
       (error "Reference binding (.= name) expects 1 name, got ~s" lhs))
     (values (goose-symbol-name (second lhs)) ".="))
    ((and (consp lhs) (sym-eq (car lhs) "TUPLE"))
     (unless (>= (length lhs) 2)
       (error "TUPLE binding expects at least 1 variable, got ~s" lhs))
     (values (format nil "~{~a~^, ~}" (mapcar #'goose-symbol-name (cdr lhs))) "="))
    (t (error "Invalid binding target ~s" lhs))))

(defun emit-single-decl (kw binding)
  "Emit a single Goose let/var/const declaration line (without trailing newline)."
  (unless (consp binding)
    (error "Binding entry must be a list, got ~s" binding))
  (let* ((lhs (first binding))
         (has-init (>= (length binding) 2))
         (init (second binding))
         (type-spec (third binding)))
    (when (> (length binding) 3)
      (error "Too many elements in binding ~s" binding))
    (multiple-value-bind (lhs-str op-str) (emit-binding-lhs lhs)
      (cond
        ((and has-init (null init) type-spec)
         ;; Uninitialized typed declaration: (x nil i64) -> let x: i64;
         (format nil "~a ~a: ~a;" kw lhs-str (emit-type type-spec)))
        ((and has-init type-spec)
         (format nil "~a ~a: ~a ~a ~a;"
                 kw lhs-str (emit-type type-spec) op-str (emit-expr init)))
        (has-init
         (format nil "~a ~a ~a ~a;" kw lhs-str op-str (emit-expr init)))
        (t
         (error "Declaration ~s requires an initializer or (name nil type)" binding))))))

(defun emit-binding-form (kw bindings body &key expr-p flat-indent)
  (let ((decl-lines (mapcar (lambda (b) (emit-single-decl kw b)) bindings)))
    (cond
      ;; No body: emit flat declaration statements
      ((null body)
       (format nil "~{~a~^~%~}" decl-lines))
      ;; Sole body inside an enclosing { ... } block: emit flat at FLAT-INDENT
      (flat-indent
       (let ((body-block (emit-body-forms body :expr-last-p expr-p :indent flat-indent)))
         (format nil "~{~a~%~}~a"
                 (mapcar (lambda (l) (concatenate 'string (indent-str flat-indent) l)) decl-lines)
                 body-block)))
      ;; Nested lexical block inside do0 or alongside sibling statements: wrap in { ... }
      (t
       (let ((inner (with-output-to-string (out)
                      (dolist (l decl-lines)
                        (format out "~a~a~%" (indent-str 1) l))
                      (write-string (emit-body-forms body :expr-last-p expr-p :indent 1) out))))
         (format nil "{~%~a}" inner))))))

;;; ---------------------------------------------------------------------------
;;; Control Flow: if, cond, when, unless, match, loop, while, for, guard, defer
;;; ---------------------------------------------------------------------------

(defun emit-block-body (forms &key expr-p)
  "Emit { ... } with INDENT around FORMS."
  (if (null forms)
      "{}"
      (let* ((single-expr (and expr-p
                               (= (length forms) 1)
                               (not (statement-only-form-p (first forms)))))
             (inline-ok (and single-expr
                             (not (block-ended-expr-p (first forms)))
                             (let ((s (emit-expr (first forms))))
                               (when (and (< (length s) 64)
                                          (not (find #\Newline s)))
                                 s)))))
        (if inline-ok
            (format nil "{ ~a }" inline-ok)
            (let ((inner (emit-body-forms forms :expr-last-p expr-p :indent 1)))
              (format nil "{~%~a}" inner))))))

(defun emit-block-like (prefix forms &key expr-p)
  (let ((body (emit-block-body forms :expr-p expr-p)))
    (if prefix
        (format nil "~a ~a" prefix body)
        body)))

(defun emit-if (form &key expr-p)
  (destructuring-bind (_ test then-form &optional (else-form nil else-p)) form
    (declare (ignore _))
    (let ((then-forms (if (and (consp then-form) (sym-eq (car then-form) "DO0"))
                          (cdr then-form)
                          (list then-form))))
      (if else-p
          (if (and (consp else-form) (sym-eq (car else-form) "IF"))
              (format nil "if ~a ~a else ~a"
                      (emit-expr test)
                      (emit-block-body then-forms :expr-p expr-p)
                      (emit-if else-form :expr-p expr-p))
              (let ((else-forms (if (and (consp else-form) (sym-eq (car else-form) "DO0"))
                                    (cdr else-form)
                                    (list else-form))))
                (format nil "if ~a ~a else ~a"
                        (emit-expr test)
                        (emit-block-body then-forms :expr-p expr-p)
                        (emit-block-body else-forms :expr-p expr-p))))
          (format nil "if ~a ~a"
                  (emit-expr test)
                  (emit-block-body then-forms :expr-p nil))))))

(defun emit-cond (form &key expr-p)
  (let ((clauses (cdr form)))
    (when (null clauses)
      (error "COND requires at least one clause"))
    (with-output-to-string (out)
      (loop for (clause . rest) on clauses
            for first-p = t then nil
            for test = (first clause)
            for body = (rest clause)
            do (cond
                 ((and (null rest) (or (eq test t) (sym-eq test "OTHERWISE") (sym-eq test "ELSE")))
                  (format out " else ~a" (emit-block-body body :expr-p expr-p)))
                 (first-p
                  (format out "if ~a ~a"
                          (emit-expr test)
                          (emit-block-body body :expr-p expr-p)))
                 (t
                  (format out " else if ~a ~a"
                          (emit-expr test)
                          (emit-block-body body :expr-p expr-p))))))))

(defun emit-match-pattern (pat)
  (cond
    ((sym-eq pat "_") "_")
    ((symbolp pat) (goose-symbol-name pat))
    ((integerp pat) (format nil "~d" pat))
    ((consp pat)
     (let ((k (sym-key (car pat))))
       (cond
         ((string= k "RANGE")
          (emit-expr pat))
         ((string= k "OR")
          (format nil "~{~a~^, ~}" (mapcar #'emit-match-pattern (cdr pat))))
         ((= (length pat) 2)
          (format nil "~a ~a"
                  (goose-symbol-name (first pat))
                  (goose-symbol-name (second pat))))
         (t (error "Invalid match pattern ~s" pat)))))
    (t (error "Invalid match pattern ~s" pat))))

(defun emit-match-arm (arm &key expr-p)
  (cond
    ((and (consp arm) (sym-member (car arm) '("CONST-FOR" "CONST-IF" "CONST-COND")))
     (let ((*match-context-p* t))
       (emit-stmt-raw arm)))
    ((consp arm)
     (let ((pat (emit-match-pattern (first arm)))
           (body (rest arm)))
       (cond
         ((null body)
          (format nil "~a => {}," pat))
         ((and (= (length body) 1)
               (consp (first body))
               (sym-eq (car (first body)) "SCOPE")
               (null (cdr (first body))))
          (format nil "~a => {}," pat))
         ((and (= (length body) 1)
               (not (statement-only-form-p (first body))))
          (format nil "~a => ~a," pat (emit-expr (first body))))
         (t
          (format nil "~a => ~a," pat (emit-block-body body :expr-p expr-p))))))
    (t (error "Invalid match arm ~s" arm))))

(defun emit-match (form &key expr-p)
  (let ((scrutinee (second form))
        (arms (cddr form)))
    (let ((inner (with-output-to-string (out)
                   (dolist (arm arms)
                     (write-string (indent-lines (emit-match-arm arm :expr-p expr-p) 1) out)
                     (write-char #\Newline out)))))
      (format nil "match ~a {~%~a}" (emit-expr scrutinee) inner))))

(defun emit-for-header (spec)
  "Parse (for (<binder> <seq> [<type>]) ...) header into Goose 'for ...'."
  (unless (and (consp spec) (<= 2 (length spec) 3))
    (error "FOR spec must be (binder seq [type]), got ~s" spec))
  (let* ((binder (first spec))
         (seq (second spec))
         (type-spec (third spec))
         (binder-str
          (cond
            ((or (symbolp binder) (stringp binder)) (goose-symbol-name binder))
            ((and (consp binder) (sym-eq (car binder) "TUPLE"))
             (format nil "~{~a~^, ~}" (mapcar #'goose-symbol-name (cdr binder))))
            (t (error "Invalid FOR binder ~s" binder)))))
    (if type-spec
        (format nil "for ~a: ~a in ~a" binder-str (emit-type type-spec) (emit-expr seq))
        (format nil "for ~a in ~a" binder-str (emit-expr seq)))))

(defun emit-guard (form)
  (let* ((cond-expr (second form))
         (rest-args (cddr form))
         (else-pos (position-if (lambda (x) (sym-eq x "ELSE")) rest-args)))
    (if else-pos
        (let ((else-forms (subseq rest-args (1+ else-pos))))
          (format nil "guard ~a else ~a"
                  (emit-expr cond-expr)
                  (emit-block-body else-forms :expr-p nil)))
        (format nil "guard ~a;" (emit-expr cond-expr)))))

(defun emit-defer (form)
  (let ((body (cdr form)))
    (when (null body)
      (error "DEFER requires at least 1 statement"))
    (if (and (= (length body) 1) (not (block-ended-expr-p (first body))))
        (format nil "defer ~a" (emit-stmt-raw (first body)))
        (format nil "defer ~a" (emit-block-body body :expr-p nil)))))

;;; ---------------------------------------------------------------------------
;;; const-for and const-if / const-cond
;;; ---------------------------------------------------------------------------

(defun emit-const-seq (seq)
  (cond
    ((and (consp seq) (sym-eq (car seq) "FIELDS"))
     (format nil "fields<~a>" (emit-type (second seq))))
    ((and (consp seq) (sym-eq (car seq) "VARIANTS"))
     (format nil "variants<~a>" (emit-type (second seq))))
    ((and (consp seq) (sym-eq (car seq) "TYPES"))
     (format nil "types<~{~a~^, ~}>" (mapcar #'emit-type (cdr seq))))
    (t (emit-expr seq))))

(defun emit-const-for (form)
  (destructuring-bind (_ spec &rest body) form
    (declare (ignore _))
    (unless (and (consp spec) (= (length spec) 2))
      (error "CONST-FOR spec must be (binder seq), got ~s" spec))
    (let* ((binder (first spec))
           (seq (second spec))
           (vars (cond
                   ((or (symbolp binder) (stringp binder)) (list (goose-symbol-name binder)))
                   ((and (consp binder) (sym-eq (car binder) "TUPLE"))
                    (mapcar #'goose-symbol-name (cdr binder)))
                   (t (error "Invalid CONST-FOR binder ~s" binder))))
           (header (format nil "const for <~{~a~^, ~}> in ~a"
                           vars (emit-const-seq seq))))
      (cond
        (*make-context-p*
         (let ((has-nested (some (lambda (f)
                                   (and (consp f)
                                        (sym-member (car f) '("CONST-FOR" "CONST-IF" "CONST-COND"))))
                                 body)))
           (multiple-value-bind (entries) (emit-make-entries body)
             (if has-nested
                 (format nil "~a { ~{~a~^ ~} }" header entries)
                 (format nil "~a { ~{~a~^, ~}~a }"
                         header entries (if entries "," ""))))))
        (*match-context-p*
         (let ((inner (with-output-to-string (out)
                        (dolist (arm body)
                          (write-string (indent-lines (emit-match-arm arm) 1) out)
                          (write-char #\Newline out)))))
           (format nil "~a {~%~a}" header inner)))
        (t
         (format nil "~a ~a" header (emit-block-body body :expr-p nil)))))))

(defun emit-const-if-branch (forms)
  (cond
    (*make-context-p*
     (multiple-value-bind (entries) (emit-make-entries forms)
       (format nil "{ ~{~a~^, ~}~a }" entries (if entries "," ""))))
    (*match-context-p*
     (let ((inner (with-output-to-string (out)
                    (dolist (arm forms)
                      (write-string (indent-lines (emit-match-arm arm) 1) out)
                      (write-char #\Newline out)))))
       (format nil "{~%~a}" inner)))
    (t
     (emit-block-body forms :expr-p nil))))

(defun emit-const-if (form)
  (let* ((cond-expr (second form))
         (rest-args (cddr form))
         (else-pos (position-if (lambda (x) (sym-eq x "ELSE")) rest-args))
         (then-forms (if else-pos (subseq rest-args 0 else-pos) rest-args))
         (else-forms (when else-pos (subseq rest-args (1+ else-pos))))
         (cond-str (let ((*const-cond-expr-p* t))
                     (emit-expr cond-expr))))
    (if else-forms
        (if (and (= (length else-forms) 1)
                 (consp (first else-forms))
                 (sym-eq (car (first else-forms)) "CONST-IF"))
            (format nil "const if ~a ~a else ~a"
                    cond-str
                    (emit-const-if-branch then-forms)
                    (emit-const-if (first else-forms)))
            (format nil "const if ~a ~a else ~a"
                    cond-str
                    (emit-const-if-branch then-forms)
                    (emit-const-if-branch else-forms)))
        (format nil "const if ~a ~a"
                cond-str
                (emit-const-if-branch then-forms)))))

(defun emit-const-cond (form)
  (let ((clauses (cdr form)))
    (when (null clauses)
      (error "CONST-COND requires at least one clause"))
    (with-output-to-string (out)
      (loop for (clause . rest) on clauses
            for first-p = t then nil
            for test = (first clause)
            for body = (rest clause)
            do (cond
                 ((and (null rest) (or (eq test t) (sym-eq test "OTHERWISE") (sym-eq test "ELSE")))
                  (format out " else ~a" (emit-const-if-branch body)))
                 (t
                  (let ((cond-str (let ((*const-cond-expr-p* t))
                                    (emit-expr test))))
                    (format out "~a~a ~a"
                            (if first-p "const if " " else const if ")
                            cond-str
                            (emit-const-if-branch body)))))))))

;;; ---------------------------------------------------------------------------
;;; Top-level declarations: defstruct, defroot, defenum, defun, globals
;;; ---------------------------------------------------------------------------

(defun parse-struct-header (name-spec clauses)
  "Extract struct name string, generic parameters, and field clauses."
  (let* ((generics nil)
         (fields clauses)
         (name-str (if (and (consp name-spec) (not (sym-eq (car name-spec) "NS")))
                       (progn
                         (setf generics (mapcar #'goose-symbol-name (cdr name-spec)))
                         (emit-type (first name-spec)))
                       (emit-type name-spec))))
    (when (and (consp (first fields)) (sym-eq (car (first fields)) "DECLARE"))
      (dolist (d (cdr (pop fields)))
        (when (and (consp d) (sym-eq (car d) "GENERICS"))
          (setf generics (append generics (mapcar #'goose-symbol-name (cdr d)))))))
    (values name-str generics fields)))

(defun emit-struct-field (field)
  (unless (consp field)
    (error "Struct field must be a list, got ~s" field))
  (cond
    ((sym-eq (first field) "PAD")
     (if (= (length field) 1)
         "pad"
         (format nil "pad ~a" (emit-expr (second field)))))
    (t
     (destructuring-bind (fname ftype &rest opts) field
       (let ((prefix "")
             (default-str nil))
         (loop for (k v) on opts by #'cddr do
           (cond
             ((sym-eq k "LET")     (when v (setf prefix "let ")))
             ((sym-eq k "CONST")   (when v (setf prefix "const ")))
             ((sym-eq k "DEFAULT") (setf default-str (emit-expr v)))
             (t (error "Unknown struct field option ~s in ~s" k field))))
         (if default-str
             (format nil "~a~a: ~a = ~a"
                     prefix (goose-symbol-name fname) (emit-type ftype) default-str)
             (format nil "~a~a: ~a"
                     prefix (goose-symbol-name fname) (emit-type ftype))))))))

(defun emit-defstruct (form &key root-p)
  (multiple-value-bind (name-str generics fields)
      (parse-struct-header (second form) (cddr form))
    (let ((gen-str (if generics (format nil "<~{~a~^, ~}>" generics) ""))
          (kw (if root-p "root struct" "struct"))
          (field-strs (mapcar #'emit-struct-field fields)))
      (if (and (not root-p)
               (<= (length field-strs) 4)
               (< (reduce #'+ field-strs :key #'length :initial-value 0) 64))
          (format nil "~a ~a~a { ~{~a~^, ~} }" kw name-str gen-str field-strs)
          (format nil "~a ~a~a {~%~{~a~a,~%~}}"
                  kw name-str gen-str
                  (mapcan (lambda (f) (list (indent-str 1) f))
                          field-strs))))))

(defun emit-enum-variant (v)
  (cond
    ((or (symbolp v) (stringp v)) (goose-symbol-name v))
    ((consp v)
     (let ((vname (goose-symbol-name (first v)))
           (fields (mapcar #'emit-struct-field (rest v))))
       (if (null fields)
           vname
           (format nil "~a { ~{~a~^, ~} }" vname fields))))
    (t (error "Invalid enum variant ~s" v))))

(defun emit-defenum (form)
  (multiple-value-bind (name-str generics variants)
      (parse-struct-header (second form) (cddr form))
    (let ((gen-str (if generics (format nil "<~{~a~^, ~}>" generics) ""))
          (var-strs (mapcar #'emit-enum-variant variants)))
      (if (and (<= (length var-strs) 4)
               (< (reduce #'+ var-strs :key #'length :initial-value 0) 64))
          (format nil "enum ~a~a { ~{~a~^, ~} }" name-str gen-str var-strs)
          (format nil "enum ~a~a {~%~{~a~a,~%~}}"
                  name-str gen-str
                  (mapcan (lambda (v) (list (indent-str 1) v))
                          var-strs))))))

(defun parse-defun-lambda-list (params)
  "Parse CL lambda list (a b &optional (c 0)) into list of (name default-p default-form)."
  (let ((result '())
        (optional-p nil))
    (dolist (p params)
      (cond
        ((sym-eq p "&OPTIONAL")
         (setf optional-p t))
        ((sym-member p '("&REST" "&KEY" "&BODY" "&AUX" "&ALLOW-OTHER-KEYS"))
         (error "Unsupported lambda-list keyword ~s in Goose defun ~s" p params))
        ((and optional-p (consp p))
         (unless (= (length p) 2)
           (error "&optional parameter in Goose requires (name default), got ~s" p))
         (push (list (first p) t (second p)) result))
        (optional-p
         (error "&optional parameter ~s in Goose requires a default value (name default)" p))
        ((symbolp p)
         (push (list p nil nil) result))
        (t (error "Invalid parameter specification ~s in ~s" p params))))
    (nreverse result)))

(defun parse-defun-declares (body)
  "Consume leading (declare ...) forms and return (values plist remaining-body)."
  (let ((type-map (make-hash-table :test #'equal))
        (var-set (make-hash-table :test #'equal))
        (values-types nil)
        (generics nil)
        (checkfree nil)
        (simd nil)
        (inline-p nil)
        (recursive nil)
        (thread-fn nil)
        (export-opt nil)
        (extern-opt nil)
        (rest-body body))
    (loop while (and (consp (first rest-body))
                     (sym-eq (car (first rest-body)) "DECLARE"))
          do (let ((clauses (cdr (pop rest-body))))
               (dolist (cl clauses)
                 (unless (consp cl)
                   (error "Invalid DECLARE clause ~s" cl))
                 (let ((ck (sym-key (car cl)))
                       (cargs (cdr cl)))
                   (cond
                     ((string= ck "TYPE")
                      (when (< (length cargs) 2)
                        (error "DECLARE TYPE needs (type T var...), got ~s" cl))
                      (let ((ty (first cargs)))
                        (dolist (v (rest cargs))
                          (setf (gethash (string-upcase (symbol-name v)) type-map) ty))))
                     ((string= ck "VAR")
                      (dolist (v cargs)
                        (setf (gethash (string-upcase (symbol-name v)) var-set) t)))
                     ((string= ck "VALUES")
                      (setf values-types cargs))
                     ((string= ck "GENERICS")
                      (setf generics (append generics (mapcar #'goose-symbol-name cargs))))
                     ((string= ck "CHECKFREE") (setf checkfree t))
                     ((string= ck "SIMD")      (setf simd t))
                     ((string= ck "INLINE")    (setf inline-p t))
                     ((string= ck "RECURSIVE") (setf recursive t))
                     ((string= ck "THREAD-FN") (setf thread-fn t))
                     ((string= ck "EXPORT")
                      (setf export-opt (if cargs (first cargs) t)))
                     ((string= ck "EXTERN")
                      (setf extern-opt (if cargs (first cargs) t)))
                     (t (error "Unknown DECLARE clause ~s in Goose defun" cl)))))))
    (values (list :type-map type-map
                  :var-set var-set
                  :values values-types
                  :generics generics
                  :checkfree checkfree
                  :simd simd
                  :inline inline-p
                  :recursive recursive
                  :thread-fn thread-fn
                  :export export-opt
                  :extern extern-opt)
            rest-body)))

(defun emit-defun (form &key inside-extern-lib-p)
  (destructuring-bind (_ name-spec params &rest raw-body) form
    (declare (ignore _))
    (let ((parsed-params (parse-defun-lambda-list params)))
      (multiple-value-bind (meta body) (parse-defun-declares raw-body)
        (let* ((name-str (emit-type name-spec))
               (type-map (getf meta :type-map))
               (var-set (getf meta :var-set))
               (ret-types (getf meta :values))
               (generics (getf meta :generics))
               (export-opt (getf meta :export))
               (extern-opt (getf meta :extern))
               (qualifiers
                (remove nil
                        (list
                         (cond
                           (inside-extern-lib-p nil)
                           ((stringp export-opt) (format nil "export ~a" (escape-goose-string export-opt)))
                           (export-opt "export")
                           (extern-opt "extern"))
                         (when (getf meta :checkfree) "checkfree")
                         (when (getf meta :simd) "simd")
                         (when (getf meta :inline) "inline")
                         (when (getf meta :recursive) "recursive"))))
               (fn-kw (if (getf meta :thread-fn) "thread_fn" "fn"))
               (gen-str (if generics (format nil "<~{~a~^, ~}>" generics) ""))
               (param-strs
                (mapcar (lambda (p)
                          (destructuring-bind (psym has-def def-val) p
                            (let* ((ukey (string-upcase (symbol-name psym)))
                                   (pname (goose-symbol-name psym))
                                   (ptype (gethash ukey type-map))
                                   (var-p (gethash ukey var-set))
                                   (prefix (if var-p "var " "")))
                              (cond
                                ((and ptype has-def)
                                 (format nil "~a~a: ~a = ~a"
                                         prefix pname (emit-type ptype) (emit-expr def-val)))
                                (ptype
                                 (format nil "~a~a: ~a" prefix pname (emit-type ptype)))
                                (has-def
                                 (format nil "~a~a = ~a" prefix pname (emit-expr def-val)))
                                (t
                                 (format nil "~a~a" prefix pname))))))
                        parsed-params))
               (ret-str (if ret-types
                            (format nil " -> ~{~a~^, ~}" (mapcar #'emit-type ret-types))
                            ""))
               (sig (format nil "~{~a ~}~a ~a~a(~{~a~^, ~})~a"
                            qualifiers fn-kw name-str gen-str param-strs ret-str)))
          (cond
            ((or extern-opt inside-extern-lib-p)
             (if (stringp extern-opt)
                 (format nil "~a = ~a;" sig (escape-goose-string extern-opt))
                 (format nil "~a;" sig)))
            (t
             (let ((expr-last-p (or (not (null ret-types))
                                    (and (= (length body) 1)
                                         (not (statement-only-form-p (first body)))))))
               (if (null body)
                   (format nil "~a {}" sig)
                   (let ((inner (emit-body-forms body :expr-last-p expr-last-p :indent 1)))
                     (format nil "~a {~%~a}" sig inner)))))))))))

(defun emit-global-decl (kw args)
  (let* ((lhs (first args))
         (has-init (>= (length args) 2))
         (init (second args))
         (type-spec (third args)))
    (multiple-value-bind (lhs-str op-str) (emit-binding-lhs lhs)
      (cond
        ((and has-init (null init) type-spec)
         (format nil "~a ~a: ~a;" kw lhs-str (emit-type type-spec)))
        ((and has-init type-spec)
         (format nil "~a ~a: ~a ~a ~a;" kw lhs-str (emit-type type-spec) op-str (emit-expr init)))
        (has-init
         (format nil "~a ~a ~a ~a;" kw lhs-str op-str (emit-expr init)))
        (t (error "Global declaration ~s ~s needs initializer or type" kw args))))))

;;; ---------------------------------------------------------------------------
;;; Statements & Sequences
;;; ---------------------------------------------------------------------------

(defun emit-stmt-raw (form)
  "Emit a single statement/top-level form without leading indentation or trailing newline."
  (cond
    ((not (consp form))
     (format nil "~a;" (emit-expr form)))
    ((and (consp (car form)) (sym-eq (caar form) "INST"))
     (format nil "~a;" (emit-expr form)))
    ((not (symbolp (car form)))
     (error "Invalid statement head ~s" form))
    (t
     (let ((k (sym-key (car form)))
           (args (cdr form)))
       (cond
         ;; Comments & whitespace
         ((string= k "SPACE") "")
         ((string= k "COMMENTS")
          (comment-newlines (first args) "// "))
         ((string= k "BLOCK-COMMENT")
          (format nil "/* ~a */" (first args)))
         ((string= k "RAW")
          (first args))

         ;; Sequential container
         ((string= k "DO0")
          (let ((lines (loop for f in args
                             collect (emit-stmt-raw f))))
            (format nil "~{~a~^~%~}" lines)))

         ;; Imports & Namespaces & Type aliases
         ((string= k "IMPORT")
          (let ((mod (first args))
                (from-pos (position-if (lambda (x) (sym-eq x "FROM")) (rest args))))
            (if from-pos
                (format nil "import ~a from ~a;"
                        (goose-symbol-name mod)
                        (escape-goose-string (nth (+ 2 from-pos) args)))
                (format nil "import ~a;" (goose-symbol-name mod)))))

         ((string= k "NAMESPACE")
          (format nil "namespace ~a;" (goose-symbol-name (first args))))

         ((string= k "NAMESPACE-ALIAS")
          (format nil "namespace ~a = ~a;"
                  (goose-symbol-name (first args))
                  (goose-symbol-name (second args))))

         ((string= k "DEFTYPE")
          (format nil "type ~a = ~a;"
                  (goose-symbol-name (first args))
                  (emit-type (second args))))

         ;; Top-level declarations
         ((string= k "DEFSTRUCT")
          (emit-defstruct form :root-p nil))
         ((string= k "DEFROOT")
          (emit-defstruct form :root-p t))
         ((string= k "DEFENUM")
          (emit-defenum form))
         ((string= k "DEFUN")
          (emit-defun form))

         ((string= k "EXTERN-LIB")
          (let* ((lib-name (first args))
                 (decls (rest args))
                 (inner (with-output-to-string (out)
                          (dolist (d decls)
                            (write-string (indent-lines (emit-defun d :inside-extern-lib-p t) 1) out)
                            (write-char #\Newline out)))))
            (format nil "extern ~a {~%~a}"
                    (escape-goose-string lib-name)
                    inner)))

         ((string= k "DEFPARAMETER") (emit-global-decl "var" args))
         ((string= k "DEFCONSTANT")  (emit-global-decl "let" args))
         ((string= k "DEFCONST")     (emit-global-decl "const" args))
         ((string= k "REUSABLE-VAR")
          (emit-global-decl "reusable var" args))
         ((string= k "REUSABLE-SLICE-VAR")
          (emit-global-decl "reusable[] var" args))

         ;; Bindings
         ((or (string= k "LET") (string= k "LET*"))
          (emit-binding-form "let" (first args) (rest args)))
         ((string= k "VAR")
          (emit-binding-form "var" (first args) (rest args)))
         ((string= k "CONST")
          (emit-binding-form "const" (first args) (rest args)))

         ;; Assignments & Increments
         ((string= k "SETF")
          (unless (= (length args) 2)
            (error "SETF expects 2 arguments, got ~s" form))
          (let ((lhs (first args))
                (rhs (second args)))
            (if (and (consp lhs) (sym-eq (car lhs) "TUPLE"))
                (format nil "~{~a~^, ~} = ~a;"
                        (mapcar #'goose-symbol-name (cdr lhs))
                        (emit-expr rhs))
                (format nil "~a = ~a;" (emit-expr lhs) (emit-expr rhs)))))

         ((string= k ".=")
          (unless (= (length args) 2)
            (error ".= expects 2 arguments, got ~s" form))
          (format nil "~a .= ~a;" (emit-expr (first args)) (emit-expr (second args))))

         ((string= k "INCF")
          (case (length args)
            (1 (format nil "~a++;" (emit-expr (first args))))
            (2 (format nil "~a += ~a;" (emit-expr (first args)) (emit-expr (second args))))
            (otherwise (error "INCF expects 1 or 2 arguments, got ~s" form))))

         ((string= k "DECF")
          (case (length args)
            (1 (format nil "~a--;" (emit-expr (first args))))
            (2 (format nil "~a -= ~a;" (emit-expr (first args)) (emit-expr (second args))))
            (otherwise (error "DECF expects 1 or 2 arguments, got ~s" form))))

         ((member k '("+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=") :test #'string=)
          (unless (= (length args) 2)
            (error "~a expects 2 arguments, got ~s" k form))
          (format nil "~a ~a ~a;" (emit-expr (first args)) (string-downcase k) (emit-expr (second args))))

         ;; Control flow statements
         ((string= k "IF")     (emit-if form :expr-p nil))
         ((string= k "WHEN")
          (format nil "if ~a ~a"
                  (emit-expr (first args))
                  (emit-block-body (rest args) :expr-p nil)))
         ((string= k "UNLESS")
          (format nil "if !~a ~a"
                  (emit-expr-prec (first args) 11)
                  (emit-block-body (rest args) :expr-p nil)))
         ((string= k "COND")   (emit-cond form :expr-p nil))
         ((string= k "MATCH")  (emit-match form :expr-p nil))
         ((string= k "SCOPE")  (emit-block-like nil args :expr-p nil))
         ((string= k "BLOCK")  (emit-block-like "block" args :expr-p nil))
         ((string= k "GUARD")  (emit-guard form))
         ((string= k "DEFER")  (emit-defer form))

         ((string= k "LOOP")
          (format nil "loop ~a" (emit-block-body args :expr-p nil)))

         ((string= k "WHILE")
          (format nil "while ~a ~a"
                  (emit-expr (first args))
                  (emit-block-body (rest args) :expr-p nil)))

         ((or (string= k "FOR") (string= k "DOTIMES") (string= k "DOLIST"))
          (format nil "~a ~a"
                  (emit-for-header (first args))
                  (emit-block-body (rest args) :expr-p nil)))

         ((string= k "CONST-FOR")  (emit-const-for form))
         ((string= k "CONST-IF")   (emit-const-if form))
         ((string= k "CONST-COND") (emit-const-cond form))

         ((string= k "BREAK")
          (if args
              (format nil "break ~a;" (emit-expr (first args)))
              "break;"))

         ((string= k "CONTINUE") "continue;")

         ((string= k "RETURN")
          (cond
            ((null args) "return;")
            ((and (= (length args) 1)
                  (consp (first args))
                  (sym-eq (car (first args)) "VALUES"))
             (let ((vals (cdr (first args))))
               (if (null vals)
                   "return;"
                   (format nil "return ~{~a~^, ~};" (mapcar #'emit-expr vals)))))
            ((= (length args) 1)
             (format nil "return ~a;" (emit-expr (first args))))
            (t (error "RETURN takes 0 or 1 argument (use (values ...) for multiple), got ~s" form))))

         ((string= k "RETURN-FROM")
          (unless (>= (length args) 1)
            (error "RETURN-FROM expects (return-from fn [val]), got ~s" form))
          (let ((fn-str (emit-type (first args)))
                (val (second args)))
            (cond
              ((null (cdr args))
               (format nil "return from ~a;" fn-str))
              ((and (consp val) (sym-eq (car val) "VALUES"))
               (format nil "return ~{~a~^, ~} from ~a;"
                       (mapcar #'emit-expr (cdr val)) fn-str))
              (t
               (format nil "return ~a from ~a;" (emit-expr val) fn-str)))))

         ((string= k "WITH-BLOCK")
          (emit-with-block form))

         (t
          (format nil "~a;" (emit-expr form))))))))

(defun emit-body-forms (forms &key expr-last-p (indent 0))
  "Emit a sequence of statement/expression forms indented by INDENT levels."
  (with-output-to-string (out)
    (loop for (f . rest) on forms
          for is-last = (null rest)
          do (cond
               ;; Flatten top-level or nested DO0 cleanly
               ((and (consp f) (sym-eq (car f) "DO0"))
                (write-string (emit-body-forms (cdr f)
                                               :expr-last-p (and is-last expr-last-p)
                                               :indent indent)
                              out))
               ;; LET / VAR / CONST at sole position inside an enclosing block emits flat
               ((and (= (length forms) 1)
                     (plusp indent)
                     (consp f)
                     (sym-member (car f) '("LET" "LET*" "VAR" "CONST"))
                     (rest (cdr f)))
                (write-string (emit-binding-form
                               (cond
                                 ((sym-eq (car f) "VAR") "var")
                                 ((sym-eq (car f) "CONST") "const")
                                 (t "let"))
                               (second f) (cddr f)
                               :expr-p expr-last-p
                               :flat-indent indent)
                              out))
               ;; Trailing value expression in value-producing block
               ((and is-last expr-last-p (not (statement-only-form-p f)))
                (let ((txt (emit-expr f)))
                  (write-string (indent-lines txt indent) out)
                  (write-char #\Newline out)))
               ;; Statement
               (t
                (let ((txt (emit-stmt-raw f)))
                  (if (string= txt "")
                      (write-char #\Newline out)
                      (progn
                        (write-string (indent-lines txt indent) out)
                        (write-char #\Newline out)))))))))

;;; ---------------------------------------------------------------------------
;;; Public entry point
;;; ---------------------------------------------------------------------------

(defun emit-goose (code &key (omit-redundant-parentheses *omit-redundant-parentheses*))
  "Transpile S-expression CODE into a Goose source string.
Pass :OMIT-REDUNDANT-PARENTHESES NIL for fully-parenthesized Oracle output."
  (let ((*omit-redundant-parentheses* omit-redundant-parentheses)
        (*make-context-p* nil)
        (*match-context-p* nil))
    (cond
      ((and (consp code) (sym-eq (car code) "DO0"))
       (emit-body-forms (cdr code) :expr-last-p nil :indent 0))
      ((statement-only-form-p code)
       (let ((txt (emit-stmt-raw code)))
         (if (string= txt "") "" (format nil "~a~%" txt))))
      ((and (consp code) (sym-member (car code) '("DEFUN" "DEFSTRUCT" "DEFROOT" "DEFENUM"
                                                   "IMPORT" "NAMESPACE" "NAMESPACE-ALIAS"
                                                   "DEFTYPE" "DEFPARAMETER" "DEFCONSTANT"
                                                   "DEFCONST" "EXTERN-LIB")))
       (format nil "~a~%" (emit-stmt-raw code)))
      (t
       (emit-expr code)))))

;; Wire type size expression hook cleanly without forward-reference warnings.
(setf *type-size-expr-fn* #'emit-expr)
