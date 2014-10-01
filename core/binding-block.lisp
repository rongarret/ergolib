
(require :ergobase)
(require :globals)

;;; Binding Block -- This is a binding construct that supports a programming style
;;; that allows deeply nested bindings without having the code crawl off the right
;;; side of the screen.  The syntax is:
;;;
;;; (binding-block [binding-spec|form]* form)
;;;
;;; or
;;;
;;; (bb [binding-spec|form]* form)
;;;
;;; A binding spec is one of the following:
;;;
;;;   varname initform     ; Regular binding
;;;   :db (vars) initform  ; Destructing-bind
;;;   :mv (vars) initform  ; Multiple-value-bind
;;;   :with spec initform  ; WITH-binding (experimental -- see below)
;;;
;;; BB returns the value of the final FORM.
;;;
;;; So, for example, this code:
;;; (let ((x 1))
;;;   (destructuring-bind ((y z) (foo))
;;;     (multiple-value-bind ((a b c) (bar))
;;;       (do-something)
;;;       (with-open-file (f "foo")
;;;          (do-something-else)))))
;;;
;;; Can be rewritten as:
;;;
;;; (bb
;;;  x 1
;;;  :db (y z) (foo)
;;;  :mv (a b c) (bar)
;;;  (do-something)
;;;  :with open-file f "foo"
;;;  (do-something-else))
;;;
;;; Note that the :with clause currently assumes that it is a stand-in for a form
;;; that looks like (with-FOO (var initform) . body).  This assumption fails for e.g.
;;; with-slots and with-gensyms.  I have not yet decided how to handle this.
;;;
;;; More thgins to fix:
;;; 1. Declarations
;;; 2. Fix dynamic bindings (currently $ prefix indicated dynamic binding)
;;;

(defmacro mvbind (vars form &body body)
  (multiple-value-bind (vars decls) (convert-args vars)
    `(multiple-value-bind ,vars ,form ,decls ,@body)))

(defindent "mvbind" 2)

(defmacro dsbind (args form &body body)
  (mvbind (args decls) (convert-args args t)
    `(destructuring-bind ,args ,form ,decls ,@body)))

(defmacro binding-block (&rest stuff) `(block nil (%bb ,@stuff)))

(defmacro bb (&rest stuff) `(binding-block ,@stuff))

(defv $binding-block-clauses nil)

(defmacro %bb (&rest body)
  (mcond
   (null (rst body)) (1st body)
   (consp (1st body)) `(progn ,(1st body) (%bb ,@(rst body)))
   (not (symbolp (1st body))) (error "~S is not a valid variable name" (1st body))
   (getf $binding-block-clauses (1st body)) (funcall it body)
   (keywordp (1st body)) (error "~S is not a valid binding keyword" (1st body))
   t (let* ((var (1st body))
            (form (2nd body))
            (decls (if (dynamic-variable? var) `((declare (special ,var))) '()))
            (body (rrst body)))
       (while (and (consp (1st body)) (eq (1st (1st body)) 'declare))
         (push (pop body) decls))
       `(let ((,var ,form)) ,@decls
          (%bb ,@body)))))

(defmacro def-bb-clause (name args expansion)
  `(progn
     (setf (getf $binding-block-clauses ',name)
           (fn (body) (dsbind ,args (rst body) ,expansion)))
     ',name))

(def-bb-clause :mv (args form &body body)
  `(mvbind ,args ,form (%bb ,@body)))

(def-bb-clause :db (args form &body body)
  `(dsbind ,args ,form (%bb ,@body)))

(def-bb-clause :with (var init cleanup &body body)
  `(let ((,var ,init)) (unwind-protect (%bb ,@body) ,cleanup)))

(def-bb-clause :with-file (spec &body body)
  `(with-open-file ,spec (%bb ,@body)))

(def-bb-clause :with-slots (vars instance &body body)
  `(with-slots ,vars ,instance (%bb ,@body)))

(def-bb-clause :fn (name args fbody &body body)
  `(labels ((,name ,args ,fbody)) (%bb ,@body)))

(def-bb-clause :tr (var form &body body)
  (with-gensym tmp
    `(%bb ,var (let ((,tmp ,form))
                 (format t "~&BB: ~A = ~S" ',var ,tmp)
                 ,tmp)
          ,@body)))
