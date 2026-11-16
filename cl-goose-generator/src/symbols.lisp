;;;; src/symbols.lisp
;;;; Package-agnostic symbol helpers and readtable-independent name normalization.

(in-package #:cl-goose-generator)

(defun has-lowercase-p (str)
  "Return T if STR contains at least one lowercase alphabetic character."
  (loop for ch across str
        thereis (lower-case-p ch)))

(defun goose-symbol-name (x)
  "Normalize a symbol into a Goose identifier string without mutating strings.
- Strings are returned verbatim (so \"SIZE\" stays \"SIZE\").
- Symbols whose symbol-name contains no lowercase characters (e.g. read under
  :upcase readtable like FOO or :A, or lowercase read under :invert like 'foo)
  are downcased.
- Symbols with mixed/lowercase characters (e.g. |Point| or Point under :invert)
  preserve their exact case."
  (etypecase x
    (string x)
    (symbol
     (let ((name (symbol-name x)))
       (if (has-lowercase-p name)
           name
           (string-downcase name))))))

(defun sym-key (x)
  "Return uppercase symbol name string when X is a symbol, NIL otherwise.
Used for package-agnostic head/keyword dispatch (never EQ on foreign symbols)."
  (when (symbolp x)
    (string-upcase (symbol-name x))))

(defun sym-eq (x expected-upper)
  "Package-agnostic comparison between symbol X and uppercase string EXPECTED-UPPER."
  (and (symbolp x)
       (string= (string-upcase (symbol-name x)) expected-upper)))

(defun sym-member (x names)
  "Return matching uppercase string if symbol X matches any name in NAMES."
  (when (symbolp x)
    (let ((u (string-upcase (symbol-name x))))
      (find u names :test #'string=))))

(defun escape-goose-string (str)
  "Escape a Lisp string into a Goose double-quoted string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across str do
      (case ch
        (#\Newline (write-string "\\n" out))
        (#\Tab     (write-string "\\t" out))
        (#\Return  (write-string "\\r" out))
        (#\Null    (write-string "\\0" out))
        (#\\       (write-string "\\\\" out))
        (#\"       (write-string "\\\"" out))
        (otherwise (write-char ch out))))
    (write-char #\" out)))

(defun escape-goose-char (ch)
  "Format a Lisp character as a Goose byte/character literal."
  (case ch
    (#\Newline "'\\n'")
    (#\Tab     "'\\t'")
    (#\Return  "'\\r'")
    (#\Null    "'\\0'")
    (#\\       "'\\\\'")
    (#\'       "'\\''")
    (otherwise (format nil "'~c'" ch))))

(defun comment-newlines (text &optional (prefix "// "))
  "Split TEXT on newlines without regex dependencies, prefixing each line."
  (let ((len (length text)))
    (if (zerop len)
        prefix
        (with-output-to-string (out)
          (loop with start = 0
                for pos = (position #\Newline text :start start)
                do (write-string prefix out)
                   (write-string (subseq text start (or pos len)) out)
                when pos do (write-char #\Newline out)
                while pos
                do (setf start (1+ pos)))))))
