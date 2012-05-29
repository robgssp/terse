(in-package :terse)

;;;; Terse Interpreter

(declaim (optimize (debug 3) (safety 3)))

;;; Symbol table funcs
(defvar *symbols* (make-hash-table :test 'equal)
  "The current symbol table")

;(defmacro with-symbol-body (&body body) body)

(defun lookup-symbol (symbol)
  "Looks up a Terse symbol"
  (let ((res (gethash symbol *symbols*)))
    (if res res
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
    (setf (gethash key *symbols*) val)))

(defmacro with-symbol-level (&body body)
  (let ((oldhash (gensym)))
    `(let ((*symbols* (make-hash-table :test 'equal))
           (,oldhash *symbols*))
       (setf (gethash :parent *symbols*) ,oldhash)
       ,@body)))

;; Not much utility on its own, but useful for implementing mutable references,
;; etc.
(defclass value ()
  ((value :initarg :value :accessor value))
  (:documentation "container for all Terse values"))

(defclass settable-value (value) ())

(defun settable-p (val)
  (typep val 'settable-value))

(defgeneric call-value (val arglist)
  (:documentation "Call a Terse value like a function (for arrays, etc)"))

(defgeneric set-value (val newval)
  (:documentation "Set an lvalue"))

(defun make-value (val) (make-instance 'value :value val))

(defun has-property (property statement)
  (when (assoc property (rest statement)) t))

(defun get-property (property statement)
  "Convenience function to get a property of the statement"
  (cdr (assoc property (rest statement))))

(defun true (val)
  "Determines whether the given Terse value represents 'true'"
  (with-slots (value) val
    (when (not (and (numberp value) (= value 0))) t)))

(defun expr-p (val)
  "Returns whether the value is a standalone expression or not"
  (or (stringp val) (numberp val) (and (listp val) (keywordp (first val)))))

(defparameter *match-exprs* ())

(defun compile (sexpr)
  (dolist (i (reverse *match-exprs*))
    (when (funcall (car i) sexpr)
      (return (funcall (cdr i) sexpr)))))

(defmacro with-compiled-props (props statement &body body)
  `(let ,(mapcar #'(lambda (x)
                     `(,x (compile (get-property ,(make-keyword x) ,statement))))
                 props)
     ,@body))

(defun add-match-expr (check body)
  (push (cons check body) *match-exprs*))

(defmacro defmatchexpr (argname chk-body  stmt-body)
  "Defines an expression based on a match"
  `(add-match-expr
    (lambda (,argname) ,chk-body) (lambda (,argname) ,stmt-body)))

(defmatchexpr stmt (or (numberp stmt) (stringp stmt))
  (lambda () (make-value stmt)))

(defparameter *keyword-exprs* ())

;; Match-expr to run the keyword expressions
(defmatchexpr stmt (eql (type-of (first stmt)) 'keyword)
  (dolist (i *keyword-exprs*)
    (when (eql (first stmt) (car i))
      (return (funcall (cdr i) stmt)))))
  
(defun add-keyword-expr (keyword lambda)
  (push (cons keyword lambda) *keyword-exprs*))

(defmacro defexpr (keyw stmt-name &body body)
  `(add-keyword-expr
    ,keyw (lambda (,stmt-name) ,@body)))

(defun simple-variable-set (stmt newval)
  "Variable set when the destination is just a string"
  (let ((name (get-property :name stmt)))
    (lambda ()
      (let ((res (funcall newval)))
        (setf (lookup-symbol name) res)
        res))))

(defun complex-variable-set (stmt newval)
  "todo"
  (let ((name (compile (get-property :name stmt))))
    (lambda ()
      (set-value (funcall name) (list (funcall newval))))))

(defexpr :variable-set stmt
  (let ((newval-stmt (get-property :args stmt)))
    (when (> (length newval-stmt) 1)
      (error "Tried to set multiple values on a variable"))
    (let ((newval (compile (first newval-stmt))))
      (if (stringp (get-property :name stmt))
          (simple-variable-set stmt newval)
          (complex-variable-set stmt newval)))))

(defexpr :variable-reference stmt
  (let ((name (get-property :name stmt)))
    (lambda () (lookup-symbol name))))

(defexpr :defun stmt
  (with-compiled-props (body) stmt
    (let ((name (when (has-property :name stmt)
                  (get-property :name stmt)))
          (args (when (has-property :args stmt)
                  (get-property :args stmt))))
      (lambda ()
        ; FUN binds the function's named argument to the passed-in values,
        ; and calls the body.
        (let ((fun (lambda (&rest argvals)
                     (map nil #'(lambda (argn argv)
                                  (setf (lookup-symbol argn) argv))
                          args argvals)
                     (funcall body))))
          (when name (setf (lookup-symbol name) fun))
          fun)))))

(defexpr :funcall stmt
  (let ((args (mapcar #'compile (get-property :args stmt))))
    (lambda ()
      (with-symbol-level
        (let ((symbol (lookup-symbol (get-property :name stmt)))
              (args (mapcar #'funcall args)))
          (if (typep symbol 'value)
              (call-value symbol args)
              (apply symbol args)))))))
  
(defexpr :if stmt
  (with-compiled-props (test body) stmt
    (if (has-property :else stmt)
        (with-compiled-props (else) stmt
          (lambda ()
            (if (true (funcall test))
                (funcall body)
                (funcall else))))
        (lambda ()
          (when (true (funcall test))
            (funcall body))))))

(defexpr :progn stmt
  (let ((parts (mapcar #'compile (rest stmt))))
    (lambda () (let ((res nil))
                 (dolist (part parts res)
                   (setf res (funcall part)))))))

;;; Array classes & funcs
(defclass array-value (value) ()
  (:documentation "Value that is callable by index"))

(defclass array-subscr-value (settable-value)
  ((index :initarg :index :accessor index))
  (:documentation "Settable array subscript"))

(defmethod call-value ((val array-value) arglist)
  "Returns the given subscript of the array"
  (make-instance 'array-subscr-value
                 :value (value val) :index (value (first arglist))))

(defmethod value ((val array-subscr-value))
  "Returns the value of the array subscript, needed because there's a
layer of indirection for settability"
  (value (elt (slot-value val 'value) (index val))))

(defmethod set-value ((val array-subscr-value) newval)
  (setf (elt (slot-value val 'value) (index val)) newval))

(defexpr :array stmt
  (let ((parts (mapcar #'compile (rest stmt))))
    (lambda () (make-instance 'array-value :value (mapcar #'funcall parts)))))

(defexpr :while stmt
  (with-compiled-props (test body) stmt
    (lambda ()
      (do ((val (make-value nil)))
          ((not (true (funcall test))) val)
        (setf val (funcall body))))))
            
(defexpr :for stmt
  (with-compiled-props (init test step body) stmt
    (lambda ()
      (funcall init)
      (do ((val (make-value nil)))
          ((not (true (funcall test))) val)
        (funcall step)
        (setf val (funcall body))))))