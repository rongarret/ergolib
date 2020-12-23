(require :ergoclos)
(require :binding-block)
(require :iterators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Dictionary - abstract associative maps for common Lisp
;;;  (Why should Python programmers have all the fun?)
;;;
;;;  Copyright (c) 2008 by Ron Garret.  This code is may be
;;;  freely distributed, modified and used for any purpose provided
;;;  this copyright notice is retained.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Generic interface to associative maps
;;;
(defgeneric ref1 (map key))
(defgeneric refd (map key default))
(defgeneric del (map key))
(defgeneric setref (map key value))
(defgeneric keys (map))

(defmacro ref (map key &optional (default nil default-supplied-p))
  (if default-supplied-p
    `(bb map ,map
         key ,key
         gs (gensym)
         val (refd map key gs)
         (if (eq val gs) (setf (ref map key) ,default) val))
    `(ref1 ,map ,key)))

(defmacro ref* (thing &rest indices)
  (if indices `(ref* (ref ,thing ,(1st indices)) ,@(rst indices)) thing))

(defsetf ref setref)
(defsetf refd setrefd)
(defun setrefd (map key default value)
  (declare (ignore default))
  (setf (ref map key) value))

(defgeneric size (map))

(define-method (size (map t)) (length (keys map)))

(define-method (size (s sequence)) (length s))

(defgeneric has-key (map key))

(define-method (has-key (map t) key)
  (not (eq '#1=#.(gensym) (ref map key '#1#))))

(defgeneric copy-into (dest src))
(defmethod copy-into (dest src)
  (for (key value) in src do (setf (ref dest key) value))
  dest)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Various ways to implement associative maps
;;;

;;;  Hash tables
(define-method (ref1 (h hash-table) key) (gethash key h))
(define-method (refd (h hash-table) key default) (gethash key h default))
(define-method (del (h hash-table) key) (remhash key h))
(define-method (setref (h hash-table) key value) (setf (gethash key h) value))
(define-method (keys (h hash-table)) (loop for k being the hash-keys of h collect k))
(define-method (size (h hash-table)) (hash-table-count h))
; Hash-table iterators are defined in utilities.lisp

;;; PLists
(define-class plist plist)
(define-method (ref1 (p plist plist) key) (getf plist key))
(define-method (refd (p plist plist) key default) (getf plist key default))
(define-method (del (p plist plist) key) (remf plist key))
(define-method (setref (p plist plist) key value) (setf (getf plist key) value))
(define-method (keys (p plist plist)) (loop for x in plist by #'cddr collect x))
(define-method (size (p plist plist)) (/ (length plist) 2))
(define-method (iterator (p plist plist))
  (let ( (l plist) ) (fn () (if l (values (pop l) (pop l)) (iterend)))))

;;; ALists
(define-class alist alist)
(define-method (ref1 (a alist alist) key) (cdr (assoc key alist)))
(define-method (refd (a alist alist) key default)
  (aif (assoc key alist) (cdr it) default))
(define-method (del (a alist alist) key) (deletef key alist :key #'car))
(define-method (setref (a alist alist) key value)
  (aif (assoc key alist)
    (setf (cdr it) value)
    (push (cons key value) alist)))
(define-method (keys (a alist alist)) (fn (n) (car (nth n alist))))
(define-method (size (a alist alist)) (length alist))
(define-method (iterator (a alist alist))
  (let ( (l alist) )
    (fn () (if l (let ( (kv (pop l)) ) (values (fst kv) (rst kv))) (iterend)))))

;;; DLists
(define-class dlist keys values)
(define-method (ref1 (d dlist keys values) key) (elt values (position key keys)))
(define-method (refd (d dlist keys values) key default)
  (aif (position key keys) (elt values it) default))
(define-method (del (d dlist keys values) key)
  (aif (position key keys)
       (progn (pop (nthcdr it keys)) (pop (nthcdr it values)))))
(define-method (setref (d dlist keys values) key value)
  (setf (elt values (position key keys)) value))
(define-method (keys (d dlist keys)) keys)
(define-method (size (d dlist keys)) (length keys))
(define-method (iterator (d dlist keys values))
  (let ( (k keys) (v values) ) (fn () (if k (values (pop k) (pop v)) (iterend)))))
(define-method (copy-into (d dlist keys values) src)
  (setf keys (copy-list (keys src)))
  (setf values (mapcar (fn (k) (ref src k)) keys))
  d)


;;; Extend the dictionary metaphor to Lisp built-in types

(define-method (ref1 (l list) (n integer))
  (nth (if (< n 0) (+ (length l) n) n) l))

(define-method (setref (l list) (n integer) val)
  (setf (nth (if (< n 0) (+ (length l) n) n) l) val))

(define-method (ref1 (l list) (indices list))
  (if (rst indices)
    (ref1 (ref1 l (1st indices)) (rst indices))
    (ref1 l (1st indices))))

(define-method (setref (l list) (indices list) val)
  (if (rst indices)
    (setref (ref1 l (1st indices)) (rest indices) val)
    (setref l (1st indices) val)))

(define-method (size (s sequence)) (length s))

(define-method (ref1 (v vector) n)
  (aref v (if (< n 0) (+ (length v) n) n)))

(define-method (setref (v vector) n val)
  (setf (aref v (if (< n 0) (+ (length v) n) n)) val))


(define-method (ref1 (o standard-object) k) (slot-value o k))

(define-method (setref (o standard-object) k val) (setf (slot-value o k) val))

(define-method (keys (o standard-object))
  (mapcar 'slot-definition-name (class-slots (find-class (type-of o)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dictionaries - abstract associative maps with dynamically changeable implementations
;;;
(defv $default-dictionary-impl-type 'equal-hash-table)

(define-class dictionary (implementation
                          (make-dictionary-implementation $default-dictionary-impl-type)))

(define-method (refd (d dictionary implementation) key default)
  (refd implementation key default))

(define-method (ref1 (d dictionary implementation) key)
  (ref1 implementation key))

(define-method (del (d dictionary implementation) key)
  (del implementation key)
  d)

(define-method (setref (d dictionary implementation) key value)
  (setref implementation key value)
  value)

(define-method (size (d dictionary implementation)) (size implementation))

(define-method (keys (d dictionary implementation)) (keys implementation))

(define-method (iterator (d dictionary implementation)) (iterator implementation))

(define-method (print-object (d dictionary implementation) stream)
  (format stream "#<~A ~A { " (type-of d) (type-of implementation))
  (if (<= (size d) 10) ; should be *print-length*
    (for (k v) in implementation do (format stream "~S ~S " k v))
    (format stream "~A items " (size d)))
  (format stream "}>"))

(defun make-dictionary-implementation (impl-type)
  (case impl-type
    ((hash-table eql-hash-table) (make-hash-table))
    (weak-hash-table (make-hash-table :test 'eq :weak :value))
    (eq-hash-table (make-hash-table :test 'eq))
    (equal-hash-table (make-hash-table :test 'equal))
    (equalp-hash-table (make-hash-table :test 'equalp))
    (otherwise (make-instance impl-type))))

(define-method (change-implementation (d dictionary implementation) new-impl-type)
  (setf implementation (copy-into (make-dictionary-implementation new-impl-type)
				  implementation))
  d)

(defun plist->dictionary (l &optional (impl-type $default-dictionary-impl-type))
  (let ( (impl (make-dictionary-implementation impl-type)) )
    (loop for (k v) on l by #'cddr do (setf (ref impl k) v))
    (make-dictionary :implementation impl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Histograms
;;;
(define-class (histogram dictionary) keyfn)

(defun make-histogram (&key (implementation (make-hash-table)) keyfn)
  (make-instance 'histogram :implementation implementation :keyfn keyfn))

(define-method (add (h histogram keyfn) item)
  (bb k (if keyfn (funcall keyfn item) item)
      (setf (ref h k) (1+ (refd h k 0)))))

;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Indexes
;;;
(define-class (index dictionary) keyfn)

(defun make-index (&key (implementation (make-hash-table)) keyfn)
  (make-instance 'index :implementation implementation :keyfn keyfn))

(define-method (add (i index keyfn) item)
  (bb k (if keyfn (funcall keyfn item) item)
      (setf (ref i k) item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sets
;;;
(define-class (set dictionary))

(defun make-set (&key (implementation (make-hash-table)) (items nil))
  (bb s (make-instance 'set :implementation implementation)
      (for item in items do (add s item))
      s))

(define-method (member? (s set) item) (refd s item nil))

(define-method (add (s set) item) (setf (ref s item) item) s)

(define-print-method (set) "#<Set ~A>" (keys self))

(shadow '(union intersection))

(define-method (union (s1 set) (s2 set))
  (bb s3 (make-set)
      (for i in s1 do (add s3 i))
      (for i in s2 do (add s3 i))
      s3))

(define-method (intersection (s1 set) (s2 set))
  (make-set :items (for i in s1 if (member? s2 i) collect i)))

(define-method (members (s set)) (keys s))

(define-method (iterator (s set implementation)) (iterator implementation))

(defmacro with-scollector (var &body body)
  (with-gensyms (result item)
    `(let ( (,result (make-set)) )
       (flet ( (,var (,item) (add ,result ,item) ,item) )
         ,@body)
       ,result)))

;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Binner
;;;
(define-class (binner dictionary) keyfn bin-factory item-transform)

(defun make-binner (keyfn &key (bin-factory 'make-set) item-transform implementation)
  (make-instance 'binner
    :keyfn keyfn :bin-factory bin-factory :item-transform item-transform
    :implementation implementation))

(define-method (add (b binner keyfn bin-factory item-transform) item)
  (bb key (funcall keyfn item)
      bin (or (ref b key) (setf (ref b key) (funcall bin-factory)))
      (add bin (If item-transform (funcall item-transform item) item))))

(define-method (del (b binner keyfn) item)
  (bb key (if keyfn (funcall keyfn item) item)
      (if (has-key b key) (del (ref b key) item))))

(defun bin (collection keyfn &optional (bin-factory 'make-set))
  (bb b (make-binner keyfn :implementation (make-hash-table :test 'equal)
                     :bin-factory bin-factory)
      (for item in collection do (add b item))
      b))

;;; Top level
(defun dict (&rest args) (plist->dictionary args))
(defun hmap (&rest args) (plist->dictionary args 'hash-table))
(defun -> (&rest args) (apply 'dict args))
