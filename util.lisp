(in-package :terse)

;;; General utility functions

(declaim (optimize (debug 3) (safety 3)))

(defun name-char-p (char)
  (or (alpha-char-p char)
      (case char
        ((#\+ #\- #\* #\/ #\> #\< #\= #\%) t))))

(defun eat-name (istream)
  "Gets a name from the input stream"
  (do ((char (peek-char nil istream nil nil) (peek-char nil istream nil nil))
       (res () (push char res)))
      ((not (and (characterp char) (name-char-p char)))
       (coerce (nreverse res) 'string))
    (read-char istream)))

(defun whitespace-p (char)
  "Returns true if the character in question is whitespace"
  (or (char= #\Space char)
      (not (graphic-char-p char))))

(defun eat-whitespace (stream)
  "Removes leading whitespace from the input stream"
  (peek-char t stream nil nil))

(defun make-keyword (symb)
  (values (intern (string symb) 'keyword)))