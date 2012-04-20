;;;; Terse Interpreter

(in-package :terse)

;;; Symbol table funcs
(defvar *symbols* (make-hash-table :test 'equal)
  "The current symbol table")

(defun lookup-symbol (symbol)
  "Looks up a Terse symbol"
  (let ((symbol (gethash symbol *symbols*)))
    (if symbol symbol
        ; recurse to parent if not in current scope
        (let ((*symbols* (gethash :parent *symbols*)))
          (when *symbols* (lookup-symbol symbol))))))

(defun set-existing-symbol (val key)
  "For use by (setf lookup-symbol). Sets the symbol and returns non-nil
if the binding already exists."
  (multiple-value-bind (oldval exists)
      (gethash key *symbols*)
    (declare (ignore oldval))
    (if exists
        (setf (gethash key *symbols*) val)
        (let ((*symbols* (gethash :parent *symbols*)))
          (when *symbols* (set-existing-symbol val key))))))

(defun (setf lookup-symbol) (val key)
  "Sets a Terse symbol"
  (unless (set-existing-symbol val key)
    (setf (gethash *symbols* key) val)))

;; Not much utility on its own, but useful for implementing mutable references,
;; etc.
(defclass value ()
  ((value :initarg :value :accessor value))
  (:documentation "container for all Terse values"))

;(defun eval-statement (statement)
;  (case (car statement)
;    (

(defun eval-list (list)
  (let ((res ()))
    (dolist (i list res)
      (setf res (eval-statement i)))))