(in-package :terse)

;;; Parser functions

(declaim (optimize (debug 3) (safety 3)))

(defvar *parse-tree* ()
  "The parse tree of the target program")

(defvar *current-line* 1)

(defun parse (stream)
  "Dispatch function for all the other parsing funcs"
  (parse-whitespace stream)
  (let ((char (peek-char nil stream nil nil)))
    (case char
         (#\f (parse-defun stream))
         (#\v (parse-variable stream))
         (#\c (parse-funcall stream))
         (#\i (parse-if-statement stream))
         (#\w (parse-while-statement stream))
         (#\" (parse-string stream))
         (#\[ (parse-array stream))
         (#\( (parse-parened stream))
         (t
          (cond
            ((eql char nil) nil)
            ((digit-char-p char)
             (parse-number stream))
            (t (error (format nil "Parse failed at line ~a: ~a"
                              *current-line* char))))))))

(defun skip-comment (stream)
  (peek-char #\Newline stream nil nil))

(defun parse-whitespace (stream)
  "Skips to the next statement counting newlines, returns next non-whitespace char"
  (do ((char (peek-char nil stream nil nil) (peek-char nil stream nil nil)))
      ((or (eql char nil) (not (or (eql char #\#) (whitespace-p char)))) char)
    (when (eql char #\Newline) (incf *current-line*))
    (when (eql char #\#) (skip-comment stream))
    (read-char stream nil nil)))

(defun has-argument (stream)
  "Checks whether the previous command has an args list"
  (eql (parse-whitespace stream) #\.))

(defun parse-arguments (stream)
  "Returns a list of parsed arguments"
  (labels ((eat-additional-arguments ()
             (let ((parsed (parse stream)))
               (cons parsed
                     (when (eql (parse-whitespace stream) #\,)
                       (read-char stream)
                       (eat-additional-arguments))))))

    (parse-whitespace stream) (read-char stream)
    (eat-additional-arguments)))

(defun char-to-num (char)
  "converts numeric character to number"
  (- (char-code char) (char-code #\0)))

(defun parse-fractional-part (stream)
  "Parses the fractional part of a decimal number"
  (read-char stream)
  (let ((res 0) (digit-magnitude 1/10))
    (loop while (digit-char-p (peek-char nil stream nil nil)) do
         (progn
           (incf res (* (char-to-num (read-char stream)) digit-magnitude))
           (setf digit-magnitude (/ digit-magnitude 10))))
    res))

(defun parse-number (stream)
  "Parses a numeric value"
  (let ((res 0))
    (loop (let ((char (peek-char nil stream nil nil)))
            (if (and (characterp char) (digit-char-p char))
                (setf res (+ (* res 10)
                             (char-to-num (read-char stream))))
                (return))))
    (if (has-argument stream)
        (float (+ res (parse-fractional-part stream)))
        res)))

;; endchar of NIL will end at end-of-file
(defun parse-region (stream endchar)
  "Parses a region, ending at endchar"
  (parse-whitespace stream)
  (prog1
      (do ((char (peek-char nil stream nil nil)
                 (peek-char nil stream nil nil))
           (res nil))
          ((eql char endchar) (cons :progn (nreverse res)))
        (push (parse stream) res)
        (parse-whitespace stream))
    (unless (eql endchar nil) (read-char stream nil nil))))

(defun parse-block (stream)
  "Parses a nested code block from the stream"
  (parse-whitespace stream)
  (read-char stream)
  (parse-region stream #\\))

(defun parse-variable (stream)
  "Parses a variable declaration/definition"
  (read-char stream) (parse-whitespace stream)
  (let ((name (if (eql (peek-char nil stream) #\()
                  (parse-parened stream)
                  (eat-name stream))))
    (if (has-argument stream)
        (list :variable-set
              (cons :name name)
              (cons :args (parse-arguments stream)))
        (list :variable-reference
              (cons :name name)))))

(defun parse-funcall (stream)
  "Parses a function call and its arguments"
  (read-char stream) (parse-whitespace stream)
  `(:funcall (:name . ,(eat-name stream))
             ,@(when (has-argument stream)
                     `((:args . ,(parse-arguments stream))))))

(defun parse-defun-arguments (stream)
  "Parses arguments for name binding"
  (cons (eat-name stream)
        (when (eql (parse-whitespace stream) #\,)
          (read-char stream) ; ,
          (parse-defun-arguments stream))))

(defun parse-defun (stream)
  "Parses a function definition"
  (read-char stream) (parse-whitespace stream)
  (let ((ret (list :defun)))
    (flet ((setprop (prop val)
             (nconc ret (list (cons prop val)))))
      (when (name-char-p (parse-whitespace stream))
        (setprop :name (eat-name stream)))
      (when (has-argument stream)
        (read-char stream) ; the .
        (setprop :args (parse-defun-arguments stream)))
      (setprop :body (parse-block stream)))))

(defun parse-if-body (stream)
  "used by parse-if-statement and parse-else-statement"
  (let ((ret (list :if
                   (cons :test (parse stream))
                   (cons :body (parse-block stream)))))
    (when (eql (parse-whitespace stream) #\e)
      (nconc ret (list (cons :else (parse-else-statement stream)))))
    ret))

(defun parse-else-statement (stream)
  "Parses an else statement and its associated block"
  (read-char stream)
  ; Check whether statement is else or else-if by checking
  ; whether the next thing is a block or not
  (if (eql (parse-whitespace stream) #\\)
      (parse-block stream)
      (parse-if-body stream)))

(defun parse-if-statement (stream)
  "Parses an if statement and its associated block"
  (read-char stream)
  (parse-if-body stream))

;; Pretty simplistic atm, need to add quote escaping sometime in the future
(defun parse-string (stream)
  "Reads in a string literal"
  (read-char stream)
  (let ((strlist (loop for i = (read-char stream)
                    while (not (eql i #\")) collecting i)))
    (make-array (length strlist) :initial-contents strlist
                :adjustable t :element-type 'character)))

(defun parse-array (stream)
  (read-char stream) (parse-whitespace stream)
  (let ((body (loop while (not (eql (peek-char nil stream) #\]))
                 collect (parse stream) do (parse-whitespace stream))))
    (prog1 (cons :array body) (read-char stream))))

(defun parse-parened (stream)
  "Parses a parenthesized stream"
  (prog2 (read-char stream) (parse stream)
    (parse-whitespace stream) (read-char stream)))

(defun parse-while-statement (stream)
  "Checks whether the following statements are in argument-form (For loop)
or a single expr (While loop)"
  (read-char stream)
  (if (eql (parse-whitespace stream) #\.)
      (let ((args (parse-arguments stream)))
        (destructuring-bind (init test step) args
          (list :for (cons :init init) (cons :test test)
                (cons :step step) (cons :body (parse-block stream)))))
      (list :while (cons :test (parse stream))
            (cons :body (parse-block stream)))))

(defun parse-stream (stream)
  "The top-level parsing function"
  (setf *current-line* 1)
  (setf *parse-tree* (parse-region stream nil)))

(defun parse-from-string (string)
  (with-input-from-string (s string)
    (parse-stream s)))