(in-package :terse)

(defun test ()
  (with-open-file (file (merge-pathnames "test.ts") :direction :input)
    (parse-stream file)))