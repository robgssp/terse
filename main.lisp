(in-package :terse)

;;; Top-level

(declaim (optimize (debug 3) (safety 3)))

(defun test-parse (filename)
  (with-open-file (f (parse-namestring filename))
    (parse-stream f)))

(defun main (filename)
  (with-open-file (f (parse-namestring filename))
    (funcall (compile (parse-stream f)))))
