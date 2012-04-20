(in-package :terse)

;;; Parser functions

(defvar *parse-tree* ()
  "The parse tree of the target program")

(defvar *current-line* 1)

(defun parse-whitespace (stream)
  "Skips to the next statement counting newlines"
  (do ((char (peek-char nil stream nil nil) (peek-char nil stream nil nil)))
      ((or (eql char nil) (not (whitespace-p char))))
    (when (eql char #\Newline) (incf *current-line*))
    (read-char stream nil nil)))

(defun has-argument (stream)
  "Checks whether the previous command has an args list"
  (parse-whitespace stream)
  (eql (peek-char t stream nil nil) #\.))

;; TODO: restructure
(defun eat-additional-arguments (stream)
  "Returns comma-separated arguments after the initial one."
  (parse-whitespace stream)
  (let ((next-char (peek-char nil stream nil nil)))
    (when (eql next-char #\,)
      (read-char stream)
      (cons (parse stream) (eat-additional-arguments stream)))))

(defun parse-arguments (stream)
  "Returns a list of parsed arguments"
  (parse-whitespace stream)
  (let ((next-char (peek-char nil stream nil nil)))
    (when (eql next-char #\.)
      (read-char stream)
      (cons (parse stream) (eat-additional-arguments stream)))))
    
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
    (loop (let ((char (peek-char nil stream)))
            (if (digit-char-p char)
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
  ;(unless (eql (peek-char nil stream nil nil) endchar)
  ;  (cons (parse stream) (parse-region stream endchar))))
  (prog1
      (loop for char = (peek-char nil stream nil nil)
         while (prog1
                   (not (eql char endchar))
                 #|(sleep .2)
                 (format t "Looking for ~a, got ~a~%" endchar char)|#)
         collecting (parse stream)
         do (parse-whitespace stream))
    (unless (eq endchar nil) (read-char stream nil nil))))

(defun parse-variable (stream)
  "Parses a variable declaration/definition"
  (read-char stream) (parse-whitespace stream)
  (let ((name (eat-name stream)))
    (if (has-argument stream)
        (list :variable-set
              (cons :name name)
              (cons :args (parse-arguments stream)))
        (list :variable-reference
              (cons :name name)))))

(defun parse-funcall (stream)
  "Parses a function call and its arguments"
  (read-char stream) (parse-whitespace stream)
  (let* ((name (eat-name stream))
         (ret (list :funcall (cons :name name))))
    (when (has-argument stream)
      (nconc ret (list (cons :args (parse-arguments stream)))))
    ret))

(defun parse-block (stream)
  "Parses a nested code block from the stream"
  (read-char stream)
  (parse-region stream #\\))

(defun parse-defun (stream)
  "Parses a function definition"
  (read-char stream) (parse-whitespace stream)
  (let ((ret (list :defun
                   (cons :name (eat-name stream)))))
    (parse-whitespace stream)
    (nconc ret (list (cons :body (parse-block stream))))))

(defun parse-if-statement (stream)
  "Parses an if statement and its associated block"
  (read-char stream) (parse-whitespace stream)
  (list :if
        (cons :test (parse stream))
        (cons :body (parse-block stream))))

(defun parse-else-statement (stream)
  "Parses an else statement and its associated block"
  (read-char stream)
  ; Check whether statement is else or else-if by checking
  ; whether the next thing is a block or not
  (if (eql (peek-char t stream nil nil) #\\)
      (list :else
            (cons :body (parse-block stream)))
      (list :else-if
            (cons :test (parse stream))
            (cons :body (parse-block stream)))))

;; Pretty simplistic atm, need to add quote escaping sometime in the future 
(defun parse-string (stream)
  "Reads in a string literal"
  (read-char stream)
  (let ((strlist (loop for i = (read-char stream)
                    while (not (eql i #\")) collecting i)))
    (make-array (length strlist) :initial-contents strlist
                :adjustable t :element-type 'character)))

(defun skip-comment (stream)
  (peek-char #\Newline stream))

(defun parse (stream)
  "Dispatch function for all the other parsing funcs"
  (parse-whitespace stream)
  (let ((char (char-downcase (peek-char nil stream nil nil))))
    (format t "Ate a ~a~%" char)
    (case char
         (#\f (parse-defun stream))
         (#\v (parse-variable stream))
         (#\c (parse-funcall stream))
         (#\i (parse-if-statement stream))
         (#\e (parse-else-statement stream))
         (#\# (skip-comment stream) (parse stream))
         (#\" (parse-string stream))
         (#\. (parse-arguments stream))
         (nil nil)
         (t
          (cond
            ((digit-char-p char)
             (parse-number stream))
            (t (error (format nil "Parse failed at line ~a" *current-line*))))))))

(defun parse-stream (stream)
  "The top-level parsing function"
  (setf *parse-tree* (parse-region stream nil)))