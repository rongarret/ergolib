
(require :ergobase)
(require :globals)

;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  CLOS utilities
;;;

;;; DEFINE-CLASS is a wrapper around DEFCLASS whose syntax is a little easier to
;;; remember, at least for me.  It is inspired by the syntax of Oaklisp.  The syntax is:
;;;
;;;   (DEFINE-CLASS (class-name superclass*) . slot-specs)
;;;
;;; If there are no superclasses, then the parens can be removed from around CLASS-NAME.
;;;
;;; Each SLOT-SPEC is:
;;;
;;;   (slot-name [type] [initial-value])
;;;
;;; If neither an initial value nor a type are given then the parens can be eliminated.
;;;
;;; So in the simplest case, (define-class foo x y z) defines a class named FOO with slots
;;; X, Y and Z.
;;;
;;; DEFINE-CLASS automatically defines slot accessors for all slots.  The slot accessor
;;; for slot S in class C is called C-S.  It also defines a constructor named MAKE-C.
;;;
;;; DEFINE-CLASS has a facility for specifying METACLASS and other class options, but it's
;;; all screwed up at the moment.
;;;

(defun concatenate-symbol (&rest strings) (apply 'symcat strings))

(defun kw-split (l)
  (labels ((prefix (l suffix)
             (if (eq l suffix) '() (cons (1st l) (prefix (rst l) suffix)))))
    (let ((suffix (member-if 'keywordp (rst l))))
      (if suffix (cons (prefix l suffix) (kw-split suffix)) (list l)))))

(defun make-slotspec (slot classname &optional shared?)
  (let ((slot (if (consp slot) slot (list slot))))
    (destructuring-bind (slotname &optional initform type) slot
      `(,slotname :initarg ,(intern (symbol-name slotname) 'keyword)
                  :accessor ,(concatenate-symbol classname '- slotname)
                  :initform ,initform
                  ,@(if type `(:type ,type))
                  ,@(if shared? '(:allocation :class))))))

(defmacro define-class (name &rest args)
  (let* ((id (if (consp name) (1st name) name))
         (superclasses (if (consp name) (rst name) '()))
         (args (kw-split args))
         (metaclass (2nd (assoc :metaclass args)))
         (instance-slots (mapcar (fn (arg) (make-slotspec arg id)) (1st args)))
         (shared-slots (mapcar (fn (arg) (make-slotspec arg id t))
                               (rst (assoc :shared args))))
         (slotspecs (append instance-slots shared-slots))
         (methods (loop for spec in slotspecs
                    if (eq (7th spec) :type) collect
             (let* ((initform (7th spec))
                    (type (9th spec)))
               `(progn
                  ; This doesn't quite do the right thing
                  (unless (typep ,initform ',type)
                    (error "Initial value for ~A must be of type ~A" ',(1st spec) ',type))
                  (defmethod (setf ,(concatenate-symbol name '- (1st spec)))
                             :before
                    (new-value (c ,name))
                    (unless (typep new-value ',type)
                      (error "Value ~S must be of type ~S" new-value ',type))))))))
   `(eval-always
      (defclass ,id ,superclasses ,slotspecs ,@(if metaclass `((:metaclass ,metaclass))))
      (finalize-inheritance (find-class ',id)) ; Required for SBCL
      (defc ,(symcat "<" id ">") (find-class ',id) t)
      ,@methods
      (defun ,(concatenate-symbol 'make- id) (&rest args)
        (apply #'make-instance ',id args))
      (defun ,(concatenate-symbol id '?) (arg)
        (typep arg ',id))
      (setf (get ',id :class-slot-specs) ',slotspecs)
      (find-class ',id))))

#+APPLE-OBJC
(defmacro define-objc-class (name &rest slots)
  (unless (consp name) (setf name (list name 'ns:ns-object)))
  `(define-class ,name ,@slots :metaclass ns:+ns-object))


;;; DEFINE-METHOD is a combination of DEFMETHOD and WITH-SLOTS designed to make
;;; method definition a little more convenient for the common case where a method
;;; is qualified over a single class and you want easy access to all the slots in
;;; that class.  The syntax is:
;;;
;;;  (define-method ((method-name . qualifiers) (arg1 class . slot-names) . args) . body)
;;;
;;; If there are no qualifiers the parens around method-name can be omitted.  So, for
;;; example:
;;;
;;;   (define-method (m1 (x c s1 s2 s3) y z) ...)
;;; is the same as:
;;;
;;;   (defmethod m1 ((x c) y z) (with-slots (s1 s2 s3) x ...))
;;;
;;; The (arg class . slots) syntax can actually be used for arguments other than the
;;; first one, so DEFINE-METHOD can be used to define multimethods.  To distinguish
;;; slots from different arguments with the same name, the slot name can be specified
;;; as argname.slotname, e.g.:
;;;
;;; (define-method (m1 (c1 class1 c1.x c1.y) (c2 class1 c2.x c2.y) ...) ...)
;;;
(defun extract-declarations (body)
  (iterate loop1 ( (declarations nil) (body body) )
    (if (and (consp body)
             (or (and (stringp (car body)) (consp body)) ; Docstring
                 (and (consp (car body))
                      (eq (caar body) 'declare))))
      (loop1 (cons (car body) declarations) (cdr body))
      (values declarations body))))

(defun munge-slot (slot)
  (let* ((s (symbol-name slot))
         (n (position #\. s)))
    (if (null n) slot (list slot (intern (subseq s (1+ n)) (symbol-package slot))))))

(defun assemble-slot-specs (var class-name)
  (if (and (consp class-name) (eq (car class-name) 'eql))
    nil
    (mapcar (fn (slot)
              (list (concatenate-symbol var "." (slot-definition-name slot))
                    (slot-definition-name slot)))
            (class-slots (find-class class-name)))))

(defun munge-method-args (args)
  (iterate loop1 ((args args) (munged-args '()) (slotspecs '()))
    (cond ((null args) (values (reverse munged-args) slotspecs))
          ((atom args) (loop1 nil (list* args '&rest munged-args) slotspecs))
          ((member (car args) lambda-list-keywords)
           (values (append (reverse munged-args) args) slotspecs))
          ((atom (car args)) (loop1 (cdr args) (cons (car args) munged-args) slotspecs))
          ((null (cdr (car args)))
           (error "Bad method lambda-list element: ~S" (car args)))
          ((null (cddr (car args)))
           (loop1 (cdr args) (cons (car args) munged-args)
                  (cons (cons (caar args)
                              (assemble-slot-specs (caar args) (2nd (car args))))
                        slotspecs)))
          (t (loop1 (cdr args)
                    (cons (list (1st (car args)) (2nd (car args))) munged-args)
                    (cons (cons (caar args) (mapcar 'munge-slot (cddar args)))
                          slotspecs))))))

(defun assemble-slot-bindings (slotspecs body)
  (if (null slotspecs)
    `(progn ,@body)
    `(with-slots ,(cdar slotspecs) ,(caar slotspecs)
       ,(assemble-slot-bindings (cdr slotspecs) body))))

(defmacro define-method ((operation &rest args) &body body)
  (if (atom operation) (setf operation (list operation)))
  (multiple-value-bind (declarations body) (extract-declarations body)
    (multiple-value-bind (arglist slotspecs) (munge-method-args args)
      `(defmethod ,@operation ,arglist
         ,@declarations
         ,(assemble-slot-bindings slotspecs body)))))

(defmacro define-print-method ((class &rest ivars) &rest args)
  `(define-method (print-object (self ,class ,@ivars) stream)
     (format stream ,@args)))

(defindent "define-print-method" 1)

(defmacro define-standard-print-method (class)
  `(define-print-method (,class) "#<~:(~A~) #x~X>" ',class (sxhash self)))

(defmacro def (name &body body)
  (if (atom name)
    `(defv ,name ,@body)
    `(progn
       (define-method ,name ,@body)
       (defv ,name (function ,name)))))

(define-synonym make make-instance)
