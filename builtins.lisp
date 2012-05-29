(in-package :terse)

;;; Builtin functions for the Terse environment

(declaim (optimize (debug 3) (safety 3)))

(defmacro defbuiltin! (name args &body body)
  `(setf (lookup-symbol ,name)
         (lambda ,args
           ,@body)))

(defmacro defbuiltin (name args &body body)
  `(setf (lookup-symbol ,name)
         (lambda ,args
           (make-value (progn ,@body)))))

(defgeneric to-string (val))

(defmethod to-string ((val value))
  (with-output-to-string (s)
    (princ (value val) s)))

(defmethod to-string ((val array-value))
  (with-output-to-string (s)
    (format s "[~{~a~^ ~}]" (mapcar #'to-string (value val)))))

(setf (lookup-symbol "print")
      (lambda (val)
        (princ (to-string val))
        val))

(defbuiltin "println" (obj)
  (funcall (lookup-symbol "print") obj) (format t "~%")
  obj)

(defbuiltin "getc" () (read-char))
(defbuiltin "getl" () (read-line))

(defbuiltin "push" (newval list)
  (push newval (value list)))

(defbuiltin "pop" (list)
  (pop (value list)))

(defbuiltin "append" (list newval)
  (assert (typep (value newval) 'list))
  (nconc (value list) (value newval)))

(defmacro defpassbuiltin (symb arity)
  (let ((argsyms (loop repeat arity collect (gensym))))
    `(defbuiltin ,(string-downcase (string symb)) ,argsyms
       (let ((val (,symb ,@(loop for i in argsyms collect `(value ,i)))))
         (cond ((eq val t) 1) ((eq val nil) 0) (t val))))))

(defmacro defpassbuiltins (symbs arity)
  `(progn ,@(loop for i in symbs collect `(defpassbuiltin ,i ,arity))))

(defpassbuiltins (+ - * / expt < > = mod) 2)
(defpassbuiltin length 1)