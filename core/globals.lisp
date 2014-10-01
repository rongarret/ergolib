
#|

GLOBALS - Global Variables Done Right

See http://rondam.blogspot.com/2009/08/global-variables-done-right.html

Written by Ron Garret, released into the public domain.

Implementation note:

Whatever we use for cells, their unique association with their respective variables
must survive a round-trip through a fasl file.  There are two ways to do this, with
LOAD-TIME-VALUE or with interned symbols.  I've chosen the latter approach here
because it allows the code to be more highly optimized.
|#

(in-package :cl-user)

(defun make-cell-for-symbol (sym type)
  (make-symbol (format nil "~A (~A)" sym type)))
;  (intern (format nil "~A (~A)" sym type) (symbol-package sym)))

(defun get-dynamic-cell (symbol)
  (or (get symbol 'dynamic-cell)
      (setf (get symbol 'dynamic-cell)
            (make-cell-for-symbol symbol "dynamic"))))

(defun dbound? (s) (boundp (get-dynamic-cell s)))

(defmacro dval (var)
  "Returns the current dynamic binding of VAR, even if there is a lexical binding in scope"
  `(symbol-value ',(get-dynamic-cell var)))

(defmacro defv (var val &optional once-only?)
  "Defines VAR to be a global dynamic variable with initial value VAL"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-symbol-macro ,var (dval ,var))
     (defun ,var () ,var)
     ,(if once-only?
        `(if (dbound? ',var)
           (warn "~A is already bound and once-only is T" ',var)
           (setf (dval ,var) ,val))
        `(setf (dval ,var) ,val))
     ',var))

(defmacro dlet (bindings &body body)
  "Unconditionally create new dynamic bindings"
  (if (atom bindings) (setf bindings `((,bindings ,(pop body)))))
  (let ((vars (mapcar 'first bindings)))
    (dolist (v vars)
      (if (not (dboundp v))
        (error "~A is not a dynamic variable" v)))
    (let ((dvars (mapcar 'get-dynamic-cell vars))
          (vals (mapcar 'second bindings)))
      `(let ,(mapcar 'list dvars vals)
         (declare (special ,@dvars)) ,@body))))

(defun dboundp (var)
  (let ((e (macroexpand-1 var)))
    (and (consp e) (eq (car e) 'dval))))

;;; Lexical globals

(defun get-lexical-cell (symbol)
  (or (get symbol 'lexical-cell)
      (setf (get symbol 'lexical-cell)
            (make-cell-for-symbol symbol "lexical"))))

(defmacro lval (var)
  "Unconditionally returns the global lexical binding of VAR"
  `(symbol-value ',(get-lexical-cell var)))

(defmacro deflexical (var val)
  "Defines VAR to be a global lexical variable"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (lval ,var) ,val)
     (define-symbol-macro ,var (lval ,var))))

;;; Immutable cells

(defmacro immutable-value (var) `(lval ,var))

(define-setf-expander immutable-value (s) (error "~A is immutable" s))

(defmacro defc (var val &optional force-reassign)
  "Immutably binds VAR to VAL.  If FORCE-REASSIGN is T then VAR is forcibly reassigned."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(if force-reassign
        `(setf (lval ,var) ,val)
        `(if (boundp ',(get-lexical-cell var))
           (bb val ,val
               old-value (symbol-value ',(get-lexical-cell var))
               (unless (equalp old-value val)
                 (cerror "Reassign it" "~A is already defined as a constant with a different value." ',var)
                 (setf (lval ,var) val)))
           (setf (lval ,var) ,val)))
     (define-symbol-macro ,var (immutable-value ,var))))

(defun constant? (expr)
  (let ((e (macroexpand-1 expr)))
    (and (consp e) (eq (car e) 'immutable-value))))

; Optional, makes DEFC behave like DEFCONSTANT
#|
(define-compiler-macro immutable-value (s) `',(symbol-value (get-lexical-cell s)))
|#

(provide :globals)
