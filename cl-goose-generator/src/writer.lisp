;;;; src/writer.lisp
;;;; Safe write-source implementation with equal hash cache and disk existence check.

(in-package #:cl-goose-generator)

(defvar *file-hashes* (make-hash-table :test #'equal)
  "Maps canonical pathname strings to last successfully written Goose source strings.")

(defvar *auto-format* nil
  "When T, run `goose format` via uiop:run-program after writing a .goose file.")

(defun ensure-goose-extension (path)
  (let ((p (pathname path)))
    (if (pathname-type p)
        p
        (make-pathname :type "goose" :defaults p))))

(defun maybe-format-goose-file (path)
  (when *auto-format*
    (ignore-errors
      (uiop:run-program (list "goose" "format" (namestring path))
                        :ignore-error-status t))))

(defun write-source (path code &key (omit-redundant-parentheses *omit-redundant-parentheses*)
                                    (format *auto-format*)
                                    (force nil))
  "Emit CODE to Goose source and write it to PATH when changed or missing on disk.
Returns (values pathname written-p code-string)."
  (let* ((target (ensure-goose-extension path))
         (key (namestring target))
         (emitted (emit-goose code :omit-redundant-parentheses omit-redundant-parentheses))
         (cached (gethash key *file-hashes*))
         (exists-p (probe-file target)))
    (when (or force
              (not exists-p)
              (not (and cached (string= cached emitted))))
      (ensure-directories-exist target)
      (with-open-file (stream target
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
        (write-string emitted stream))
      (let ((*auto-format* format))
        (maybe-format-goose-file target))
      ;; Record cache ONLY after disk write succeeds (never sxhash).
      (setf (gethash key *file-hashes*) emitted)
      (return-from write-source (values target t emitted)))
    (values target nil emitted)))
