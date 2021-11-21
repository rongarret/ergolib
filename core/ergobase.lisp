
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro eval-always (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body)))

(defmacro without-length-restrictions (&body body)
  `(let ((*print-length* nil) (*print-string-length* nil))
     ,@body))

(defmacro safely (&body body)
  `(let ((*read-eval* nil)) ,@body))

(defun printl (thing &optional (stream t))
  (without-length-restrictions 
   (if stream
     (progn
       (print thing stream)
       (values))
     (prin1-to-string thing))))

(defun princl (thing &optional (stream t))
  (without-length-restrictions 
   (if stream
     (progn
       (princ thing stream)
       (values))
     (princ-to-string thing))))

(defmethod ->string (thing) (printl thing nil))
(defmethod ->string ((s string)) s)
(defmethod ->string ((c character)) (princ-to-string c))

(defun strcat (&rest things)
  (apply 'concatenate 'string (mapcar '->string things)))

(defun symcat (&rest strings)
  (intern (apply 'strcat strings)))

(defun id= (s1 s2)
  (and (or (stringp s1) (symbolp s1))
       (or (stringp s2) (symbolp s2))
       (string-equal s1 s2)))

(defmacro iterate (name args &rest body)
  `(labels ((,name ,(mapcar 'first args) ,@body))
     (,name ,@(mapcar 'second args))))

; Suppress warnings for shadowing LOOP
(shadow 'loop)
(defmacro loop (&body body) `(cl:loop ,@body))

(defun dynamic-variable? (v)
  (and (symbolp v)
       (let ( (c (elt (symbol-name v) 0)) )
         (or (eql c #\$) (eql c #\*)))))

(defun dummy-variable? (v)
  (and (symbolp v) (string= v "_")))

(defun convert-args (args &optional destructure)
  (let (ignore specials)
    (flet ((convert-arg (arg)
             (cond ((dummy-variable? arg)
                    (push (gensym "DUMMY") ignore)
                    (car ignore))
                   ((dynamic-variable? arg)
                    (push arg specials)
                    arg)
                   (t arg))))
      (values
       (iterate loop ((args args))
         (cond ((null args) nil)
               ((atom args) (list '&rest (convert-arg args)))
               ((and destructure (consp (car args)))
                (cons (loop (car args)) (loop (cdr args))))
               (t (cons (convert-arg (car args)) (loop (cdr args))))))
       `(declare (ignore ,@ignore) (special ,@specials))))))

(defmacro fn (args &body body)
  (multiple-value-bind (args decls) (convert-args args)
    `(lambda ,args ,decls ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (fn (s) (list s `(gensym ,(symbol-name s)))) syms)) ,@body))

(defmacro with-gensym (sym &body body)
  `(with-gensyms (,sym) ,@body))

(defmacro define-synonym (s1 s2)
  `(progn
     (setf (symbol-function ',s1) (symbol-function ',s2))
     (defsetf ,s1 (&rest args) (new-value)
       `(setf (apply (function ,',s2) ,args) ,new-value))))

(define-synonym fst car)
(define-synonym ffst caar)
(define-synonym fffst caaar)
(define-synonym rst cdr)
(define-synonym rrst cddr)
(define-synonym rrrst cdddr)

(defmethod ref1 ((l list) index) (nth index l))
(defmethod ref1 ((v vector) index) (elt v index))

(defmacro define-index-function (name index)
  `(defun ,name (thing) (ref1 thing ,index)))

(define-index-function 1st 0)
(define-index-function 2nd 1)
(define-index-function 3rd 2)
(define-index-function 4th 3)
(define-index-function 5th 4)
(define-index-function 6th 5)
(define-index-function 7th 6)
(define-index-function 8th 7)
(define-index-function 9th 8)
(define-index-function 10th 9)

;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collectors
;;;
(defmacro with-collector (var &body body)
  (with-gensym resultvar
    `(let ( (,resultvar '()) )
       (flet ( (,var (item) (push item ,resultvar) item) )
         ,@body)
       (nreverse ,resultvar))))

(define-symbol-macro [] (make-array 0 :fill-pointer t :adjustable t))

(defun vpe (vector item) (vector-push-extend item vector) vector)

(defmacro with-vcollector (var &body body)
  (with-gensyms (result item)
    `(let ( (,result []) )
       (flet ( (,var (,item) (vpe ,result ,item) ,item) )
         ,@body)
       ,result)))

;;; Because with-output-to-string is soooooo sloooooowwww

(defmacro with-char-collector (var &rest body)
  (with-gensym svar
    `(let ( (,svar (make-array 0 :element-type 'character
                               :fill-pointer t :adjustable t)) )
       (labels ( (,var (thing)
		   (cond ( (characterp thing) (vector-push-extend thing ,svar) )
			 ( (stringp thing)
                           (loop for c across thing do (vector-push-extend c ,svar)) )
			 (t (,var (princ-to-string thing))))) )
	 ,@body
         ,svar))))

;;; User-extendable collector

(let ( (collector-table (make-hash-table :test #'eq)) )
(flet ( (trim-symbol (symbol)
          (read-from-string (string-trim "'" (->string symbol)))) )
  
  (defmacro define-type-collector (type &body body)
    (with-gensym dummy
      (let ( (macname (trim-symbol dummy)) )
        (setf (gethash (trim-symbol type) collector-table) macname)
        `(defmacro ,macname (var &body body)
           ,@body))))
  
  (defmacro create-type-collector (type initializer adder
                                   &optional (finisher (fn (x) x)))
    (with-gensym dummy
      (let ( (macname (trim-symbol dummy)) )
        (setf (gethash (trim-symbol type) collector-table) macname)
        `(defmacro ,macname (var &body body)
           (with-gensyms (result item)
             (let ( (initializer ,initializer)
                    (adder ,adder)
                    (finisher ,finisher) )
               `(let ( (,result (funcall ,initializer)) )
                  (flet ( (,var (,item) (funcall ,adder ,result ,item) ,item) )
                    ,@body)
                  (funcall ,finisher ,result))))))))
  
  (defun define-type-collector-synonym (new existing)
    (setf (gethash (trim-symbol new) collector-table)
          (gethash (trim-symbol existing) collector-table)))
  
  (defun ref-type-collector (type)
    (let ( (macname (gethash (trim-symbol type) collector-table)) )
      (or macname
          (error "Collector for ~A is not defined" type))))))

(defun type-of1 (thing)
  (let ( (type (type-of thing)) )
    (if (atom type) type (car type))))

(defmacro with-type-collector (type var &body body)
  (let ( (macname (ref-type-collector type)) )
    `(,macname ,var ,@body)))

(defmacro with-auto-collector (thing var &body body)
  `(let ( (type (type-of1 ,thing))
          (var ',var)
          (body ',body) )
     (eval `(with-type-collector ,type ,var ,@body))))

(define-type-collector 'list `(with-collector ,var ,@body))
(define-type-collector-synonym 'cons 'list)

(defun init-vector () [])
(create-type-collector 'vector #'init-vector #'vpe)
(define-type-collector-synonym 'simple-vector 'vector)

(define-type-collector 'string `(with-char-collector ,var ,@body))
(define-type-collector-synonym 'simple-base-string 'string)

;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Anaphoric conditionals
;;;

(defmacro aif (condition &optional (then nil then-p) &rest more)
  (if then-p
    `(let ((it ,condition)) (if it ,then ,(if more `(aif ,@more))))
    condition))

(defmacro awhile (condition &body body)
  `(let ((it ,condition))
     (while it ,@body (setf it ,condition))))

;;; MCOND is named for John McCarthy, who always thought COND had too many parens
(defmacro mcond (&rest clauses)
  (if (null (cdr clauses))
    (car clauses)
    `(aif ,(1st clauses) ,(2nd clauses) (mcond ,@(rrst clauses)))))

;;; Misc utils
(defun hex (n) (format nil "~X" n))
(defun b32 (n) (let ((*print-base* 32)) (princ-to-string n)))

(defun unhex (s) (let ((*read-base* 16)) (read-from-string s)))
(defun unb32 (s) (let ((*read-base* 32)) (read-from-string s)))

(defun bytes-to-string (bytes &optional (encoding :utf-8))
  (decode-string-from-octets (coerce bytes '(vector (unsigned-byte 8)))
                             :external-format encoding))

(defun string-to-bytes (s &optional (encoding :utf-8))
  (encode-string-to-octets s :external-format encoding))

(defmacro deletef (thing place &rest args)
  `(setf ,place (delete ,thing ,place ,@args)))

(defmacro n-of (form n)
  `(loop for #.(gensym "I") from 1 to ,n collect ,form))

(defun fmt (s &rest args)
  (without-length-restrictions
   (apply 'format nil s args)))
