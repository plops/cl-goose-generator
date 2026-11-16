;;;; src/types.lisp
;;;; Goose type syntax emitter (arrays, slices, refs, relative refs, index types, const).

(in-package #:cl-goose-generator)

(defvar *type-size-expr-fn* nil
  "Hook set by emitter.lisp to format non-trivial constant expressions inside T[expr].")

(defun type-needs-postfix-parens-p (form)
  "Return T when FORM produces a Goose type that must be parenthesized before
attaching an array/slice/ref/opt postfix (e.g. (u32 in a)[>..], (Mark..&<u32 in marks>)[>..])."
  (and (consp form)
       (let ((k (sym-key (car form))))
         (or (string= k "IN")
             (string= k "UPTO")
             (string= k "FN-TYPE")
             (and (string= k "REL")
                  (find-if (lambda (x) (sym-eq x "IN")) (cdddr form)))))))

(defun emit-type-base-for-postfix (form)
  "Emit FORM, parenthesizing when a postfix modifier cannot attach directly."
  (let ((s (emit-type form)))
    (if (type-needs-postfix-parens-p form)
        (format nil "(~a)" s)
        s)))

(defun emit-type-size (sz)
  "Emit an array size/length-type specifier (integer, varint/u8/SIZE, or const expr)."
  (cond
    ((integerp sz) (format nil "~d" sz))
    ((symbolp sz) (goose-symbol-name sz))
    ((stringp sz) sz)
    ((and (consp sz) *type-size-expr-fn*)
     (funcall *type-size-expr-fn* sz))
    (t (error "Invalid array size specifier in Goose type: ~s" sz))))

(defun extract-rel-in (args)
  "Parse optional (:in pool) from REL type arguments."
  (let ((pos (position-if (lambda (x) (sym-eq x "IN")) args)))
    (when pos
      (unless (< (1+ pos) (length args))
        (error "Missing pool expression after :IN in REL type: ~s" args))
      (nth (1+ pos) args))))

(defun emit-type (form)
  "Emit a Goose type expression from an S-expression FORM."
  (cond
    ((symbolp form)
     (goose-symbol-name form))
    ((stringp form)
     form)
    ((not (consp form))
     (error "Invalid Goose type form: ~s" form))
    (t
     (let ((head (sym-key (car form)))
           (args (cdr form)))
       (cond
         ((string= head "NS")
          (destructuring-bind (ns-name sym) args
            (if (null ns-name)
                (format nil "::~a" (emit-type sym))
                (format nil "~a::~a" (goose-symbol-name ns-name) (emit-type sym)))))

         ((string= head "INST")
          (when (null args)
            (error "INST type requires at least a type name: ~s" form))
          (format nil "~a<~{~a~^, ~}>"
                  (emit-type (car args))
                  (mapcar #'emit-type (cdr args))))

         ((string= head "VAR-MODE")
          (unless (= (length args) 1)
            (error "VAR-MODE expects 1 argument, got ~s" form))
          (format nil "~a.." (emit-type (first args))))

         ((or (string= head "VARIANT-TYPE") (string= head "DOT"))
          (unless (= (length args) 2)
            (error "~a in type position expects 2 arguments (Enum Variant), got ~s"
                   head form))
          (format nil "~a.~a"
                  (emit-type (first args))
                  (goose-symbol-name (second args))))

         ((string= head "ARR")
          (case (length args)
            (1 (format nil "~a[]" (emit-type-base-for-postfix (first args))))
            (2 (format nil "~a[~a]"
                       (emit-type-base-for-postfix (first args))
                       (emit-type-size (second args))))
            (otherwise (error "ARR expects 1 or 2 arguments, got ~s" form))))

         ((string= head "CAP-ARR")
          (case (length args)
            (1 (format nil "~a[..]" (emit-type-base-for-postfix (first args))))
            (2 (format nil "~a[..~a]"
                       (emit-type-base-for-postfix (first args))
                       (emit-type-size (second args))))
            (otherwise (error "CAP-ARR expects 1 or 2 arguments, got ~s" form))))

         ((string= head "GROW")
          (unless (= (length args) 1)
            (error "GROW expects 1 argument, got ~s" form))
          (format nil "~a[>..]" (emit-type-base-for-postfix (first args))))

         ((string= head "STACK")
          (unless (= (length args) 1)
            (error "STACK expects 1 argument, got ~s" form))
          (format nil "~a[>..<]" (emit-type-base-for-postfix (first args))))

         ((string= head "SLICE")
          (unless (= (length args) 1)
            (error "SLICE expects 1 argument, got ~s" form))
          (format nil "~a[:]" (emit-type-base-for-postfix (first args))))

         ((string= head "REF")
          (unless (= (length args) 1)
            (error "REF expects 1 argument, got ~s" form))
          (format nil "~a&" (emit-type-base-for-postfix (first args))))

         ((string= head "OPT")
          (unless (= (length args) 1)
            (error "OPT expects 1 argument, got ~s" form))
          (let* ((inner (first args))
                 (s (emit-type inner)))
            (if (and (consp inner)
                     (sym-member (car inner) '("IN" "UPTO" "FN-TYPE")))
                (format nil "(~a)?" s)
                (format nil "~a?" s))))

         ((string= head "REL")
          (unless (>= (length args) 2)
            (error "REL expects (rel T width [:in pool]), got ~s" form))
          (let* ((target (first args))
                 (width (second args))
                 (pool (extract-rel-in (cddr args)))
                 (base (emit-type-base-for-postfix target))
                 (w-str (goose-symbol-name width)))
            (if pool
                (format nil "~a&<~a in ~a>"
                        base
                        w-str
                        (if *type-size-expr-fn*
                            (funcall *type-size-expr-fn* pool)
                            (emit-type pool)))
                (format nil "~a&<~a>" base w-str))))

         ((string= head "IN")
          (unless (= (length args) 2)
            (error "IN index type expects (in width array), got ~s" form))
          (format nil "~a in ~a"
                  (goose-symbol-name (first args))
                  (if *type-size-expr-fn*
                      (funcall *type-size-expr-fn* (second args))
                      (emit-type (second args)))))

         ((string= head "UPTO")
          (unless (= (length args) 2)
            (error "UPTO bound type expects (upto width array), got ~s" form))
          (format nil "~a upto ~a"
                  (goose-symbol-name (first args))
                  (if *type-size-expr-fn*
                      (funcall *type-size-expr-fn* (second args))
                      (emit-type (second args)))))

         ((string= head "CONST")
          (unless (= (length args) 1)
            (error "CONST expects 1 argument, got ~s" form))
          (let ((inner (first args)))
            (if (and (consp inner)
                     (sym-eq (car inner) "REF")
                     (consp (second inner))
                     (sym-member (car (second inner)) '("SLICE" "REF" "CONST")))
                (format nil "const (~a)&" (emit-type (second inner)))
                (format nil "const ~a" (emit-type inner)))))

         ((string= head "FN-TYPE")
          (unless (<= 1 (length args) 2)
            (error "FN-TYPE expects (fn-type (param-types...) [ret-type]), got ~s" form))
          (let ((param-types (mapcar #'emit-type (first args)))
                (ret (second args)))
            (if ret
                (format nil "fn(~{~a~^, ~}) -> ~a" param-types (emit-type ret))
                (format nil "fn(~{~a~^, ~})" param-types))))

         (t
          (error "Unknown Goose type constructor ~s in ~s" (car form) form)))))))
