
;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Mappers
;;;
(define-synonym walk mapc)

;;;  MAP-EXTEND works like mapcar except that its termination condition is when all
;;;  of its argument lists are nil.
(defun map-extend (fn &rest lists)
  (if (every #'null lists)
    nil
    (cons (apply fn (mapcar #'car lists))
          (apply #'map-extend fn (mapcar #'cdr lists)))))

;;;  MMAP is a generalized version of MAP for mapping functions which return
;;;  multiple values.
(defun mmap (fn &rest lists)
  (if (some #'null lists)
    nil
    (let* ( (cars (multiple-value-list (apply fn (mapcar #'car lists))))
            (cdrs (multiple-value-list (apply #'mmap fn (mapcar #'cdr lists)))) )
      (apply #'values (map-extend #'cons cars cdrs)))))

;;;  The following mapping functions map only their first arguments.  All their
;;;  subsequent arguements are passed unaltered to the mapping function.
;;;  e.g. (map1 + '(1 2 3) 4) => (5 6 7)
(defun map1 (fn mapped-arg &rest unmapped-args)
  (mapcar #'(lambda (arg) (apply fn arg unmapped-args)) mapped-arg))

(defun walk1 (fn mapped-arg &rest unmapped-args)
  (mapc #'(lambda (arg) (apply fn arg unmapped-args)) mapped-arg))

(defun mmap1 (fn args1 &rest unmapped-args)
  (mmap #'(lambda (arg) (apply fn arg unmapped-args)) args1))

;;; Leaf mappers
(defun walkleaves (fn tree)
  (iterate loop1 ( (tree tree) )
    (if (atom tree)
      (funcall fn tree)
      (progn (loop1 (car tree)) (and (cdr tree) (loop1 (cdr tree)))))))

(defmacro doleaves ((var tree) &body body)
  `(walkleaves (fn (,var) ,@body) ,tree))

(defun mapleaves (fn tree)
  (iterate loop1 ( (tree tree) )
    (if (atom tree)
      (funcall fn tree)
      (cons (loop1 (car tree)) (and (cdr tree) (loop1 (cdr tree)))))))

(defun mapleaves! (fn tree)
  (iterate loop1 ( (tree tree) )
    (if (atom tree)
      (funcall fn tree)
      (progn
        (setf (car tree) (loop1 (car tree)))
        (setf (cdr tree) (and (cdr tree) (loop1 (cdr tree))))
        tree))))

;;; Misc. mappers
(defun mappend (fn &rest lists) (apply #'append (apply #'mapcar fn lists)))
(define-synonym mappend! mapcan)
(defun mappend1 (fn &rest lists) (apply #'append (apply #'map1 fn lists)))

(define-synonym mapcdr maplist)
(define-synonym walkcdr mapl)

(defun map! (fn l)
  (walkcdr #'(lambda (l) (setf (car l) (funcall fn (car l)))) l))

(defmacro maplet (bindings &body body)
  `(mapcar (fn ,(mapcar #'car bindings) ,@body) ,@(mapcar #'second bindings)))

(defmacro walklet (bindings &body body)
  `(walk (fn ,(mapcar #'car bindings) ,@body) ,@(mapcar #'second bindings)))
