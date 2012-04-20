(in-package :terse)

;;; General utility functions

(defun eat-name (istream)
  "Gets a name from the input stream"
  (let ((strlist ()))
    (loop
       (let ((char (peek-char nil istream nil nil)))
         (if (and (characterp char) (alpha-char-p char))
             (push (read-char istream) strlist)
             (return))))
    (make-array (length strlist) :element-type 'character :initial-contents
                (nreverse strlist))))

(defun whitespace-p (char)
  "Returns true if the character in question is whitespace"
  (or (char= #\Space char)
      (not (graphic-char-p char))))

(defun eat-whitespace (stream)
  "Removes leading whitespace from the input stream"
  (peek-char t stream nil nil))

(defun string-to-keyword (string)
  (values (intern (string-upcase string) :keyword)))


