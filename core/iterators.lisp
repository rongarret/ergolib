(require :ergoclos)
(require :binding-block)

;;;;;;;;;;;;;;;;;;;
;;;
;;; Iterators
;;;

(defun iterend () #'iterend)
(defun iterend? (x) (eq x #'iterend))
(define-compiler-macro iterend () #'iterend)

(defmacro iterdo (vars expr &body body)
  (if (atom vars) (setf vars (list vars)))
  ; Variables named _ are ignored, but we can't ignore the first variable
  ; in this case because it controls the iteration
  (if (eq (car vars) '_) (setf (car vars) (gensym)))
  (with-gensyms (itervar loop)
    `(bb ,itervar (iterator ,expr)
         (iterate ,loop ()
           (bb :mv ,vars (funcall ,itervar)
               (if (iterend? ,(1st vars))
                 (values)
                 (progn ,@body (,loop))))))))

(defun check-keyword (kw expected &optional (function 'warn))
  (if (atom expected)
    (unless (id= kw expected)
      (funcall function "Unexpected keyword: expected ~A, got ~A" expected kw))
    (unless (member kw expected :test 'id=)
      (funcall function "Unexpected keyword ~A, expected one of ~A" kw expected))))

(defmacro yield (expr for vars in iter &optional if condition)
  (check-keyword for :for)
  (check-keyword in :in)
  (if (or if condition) (check-keyword if :if))
  (bb var1 (if (atom vars) vars (1st vars))
      vars (if (consp vars) vars (list vars))
      (if (and if (not (eq condition t)))
        `(bb iter (iterator ,iter)
             (fn ()
               (iterate loop ()
                 (bb :mv ,vars (funcall iter)
                     (mcond (iterend? ,var1) ,var1
                            ,condition ,expr
                            (loop))))))
        `(bb iter (iterator ,iter)
             (fn () (bb :mv ,vars (funcall iter)
                        (if (iterend? ,var1) ,var1 ,expr)))))))

(defmacro for (var in thing &body body)
  (check-keyword in :in)
  (bb kw (1st body)
      (if (consp kw) (setf kw :do) (pop body))
      (check-keyword kw '(:do :if :collect :vcollect :yield) 'error)
      condition t
      (mcond (id= kw :if)
             (setf condition (1st body) kw (2nd body) body (rrst body))
             (and (= (length body) 3) (id= (2nd body) :if))
             (setf condition (3rd body) body (list (1st body))))
      (check-keyword kw '(:do :collect :vcollect :yield) 'error)
      (if (and (cddr body) (not (id= kw :do)))
        (error "Multiple forms following a ~A keyword" kw))
      (push 'progn body)
      (if (id= kw :yield)
        (return `(yield ,body for ,var in ,thing if ,condition)))
      (ecase (intern (symbol-name kw) :keyword)
        (:do `(iterdo ,var ,thing (if ,condition ,body)))
        (:collect (with-gensym collect
                    `(with-collector ,collect
                       (iterdo ,var ,thing (if ,condition (,collect ,body))))))
        (:vcollect (with-gensym collect
                     `(with-vcollector ,collect
                        (iterdo ,var ,thing (if ,condition (,collect ,body)))))))))

(defun check-keyword-with-arrow (kw expected &optional (function 'warn))
  (bb s (->string kw)
      arrows '(-> -><type>)
      (if (starts-with s "->")
        (slice s 2)
        (check-keyword kw (cat expected arrows) function))))

;;; FOR with an arrow
(defmacro forw (var in thing &body body)
  (check-keyword in :in)
  (bb kw (1st body)
      (if (consp kw) (setf kw :do) (pop body))
      (check-keyword-with-arrow kw '(:do :if :yield) 'error)
      condition t
      (mcond (id= kw :if)
             (setf condition (1st body) kw (2nd body) body (rrst body))
             (and (= (length body) 3) (id= (2nd body) :if))
             (setf condition (3rd body) body (list (1st body))))
      typestr (check-keyword-with-arrow kw '(:do :yield) 'error)
      (if (and (cddr body) (not (id= kw :do)))
        (error "Multiple forms following a ~A keyword" kw))
      (push 'progn body)
      (if typestr
        (with-gensym collect
          (if (string= "" typestr)
            (return `(let ( (type (type-of1 ,thing)) (collect ',collect)
                            (var ',var) (thing ',thing)
                            (condition ',condition) (body ',body) )
                       (eval `(with-type-collector ,type ,collect
                                (iterdo ,var ,thing
                                  (if ,condition (,collect ,body)))))))
            (let ( (type (read-from-string typestr)) )
              (return `(with-type-collector ,type ,collect
                         (iterdo ,var ,thing
                           (if ,condition (,collect ,body)))))))))
      (if (id= kw :yield)
        (return `(yield ,body for ,var in ,thing if ,condition)))
      `(iterdo ,var ,thing (if ,condition ,body))))

(defmacro collect (expr for var in iter &optional if condition)
  (check-keyword for :for 'error)
  (check-keyword in :in 'error)
  (if if
    `(for ,var in ,iter collect ,expr ,if ,condition)
    `(for ,var in ,iter collect ,expr)))

(defmacro vcollect (expr for var in iter &optional if condition)
  (check-keyword for :for 'error)
  (check-keyword in :in 'error)
  (if if
    `(for ,var in ,iter vcollect ,expr ,if ,condition)
    `(for ,var in ,iter vcollect ,expr)))

(define-method (iterator (l list)) (fn () (if l (pop l) (iterend))))

(define-method (tails (l list)) (fn () (if l (prog1 l (pop l)) (iterend))))

(define-method (iterator (v vector))
  (let ( (len (length v)) (cnt 0) )
    (fn () (if (< cnt len)
             (multiple-value-prog1 (values (elt v cnt) cnt) (incf cnt))
             (iterend)))))

(define-method (iterator (f function)) f)

(define-method (iterator (s stream))
  (fn ()
    (let ((c (read-char s nil (iterend))))
      (if (eq c (iterend)) (close s))
      c)))

(define-method (iterator (p pathname))
  (let ((s (open p)))
    ; BUG: this will still leak because (open-file-streams) keeps a pointer to
    ; all open streams
    (terminate-when-unreachable s 'close)
    (iterator s)))

(defmacro define-stream-iterator (name reader)
  `(progn
     (define-class ,name stream)
     (define-method (iterator (l ,name stream))
       (fn ()
         (let ((item (,reader stream nil (iterend))))
           (if (eq item (iterend)) (close stream))
           item)))
     (define-method (,name (s stream)) (make-instance ',name :stream s))
     (define-method (,name (s string)) (make-instance ',name :stream (make-string-input-stream s)))
     (define-method (,name (p pathname))
       (let ((iter (make-instance ',name :stream (open p))))
         (terminate-when-unreachable iter)
         iter))
     (define-method (slice (x ,name) start &optional end step)
       (slice (for item in x yield item) start end step))
#+CCL ; Need to replace this with trivial-garbage
     (define-method (ccl:terminate (iter ,name stream)) (close stream))))

(define-stream-iterator lines read-line)
(define-stream-iterator forms read)

(define-method (iterator (h hash-table))
  (let ( (keys (loop for x being the hash-keys of h collect x)) )
    (fn ()
      (if keys (let ( (k (pop keys)) ) (values k (gethash k h))) (iterend)))))

(define-method (iterator (p package))
  (iterator
   (with-collector collect
     (do-symbols (s p)
       (if (eq (symbol-package s) p) (collect s))))))

(defun zip (&rest things)
  (let ( (iterators (mapcar 'iterator things)) )
    (fn () (apply 'values (mapcar 'funcall iterators)))))

; ZIPLIST is like ZIP except that it takes one list of iterables and produces
; lists of items rather than multiple values
(defun ziplist (things)
  (let ( (iterators (map 'list 'iterator things)) )
    (fn () (bb l (mapcar 'funcall iterators)
               (if (eq (car l) (iterend)) (iterend) l)))))

; VCAT is like CONCATENATE 'VECTOR except that it works on iterators
(defun vcat (&rest things) ; Like STRCAT but for vectors
  (with-vcollector collect
    (for thing in things do
      (for item in thing do
        (collect item)))))

(defun counter (&optional (start 0) end (step 1) (keyfn #'identity))
  (fn ()
    (if (and end (<= 0 (* (signum step) (- start end))))
      (iterend)
      (prog1 (funcall keyfn start) (incf start step)))))

;;; N-AT-A-TIME and SLICES do more or less the same thing, iterate over a
;;; sequence N items at a time, but N-AT-A-TIME delivers the results as
;;; multiple values and SLICES delivers them as sequences
;;;
(defun n-at-a-time (n thing)
  (let ( (iter (iterator thing)) (n (1- n)) )
    (fn () (let ((_ (funcall iter)))
             (if (iterend? _) _ (apply 'values _ (n-of (funcall iter) n)))))))

(defun slices (n thing)
  (let ( (iter (iterator thing)) (n (1- n)) )
    (fn () (let ((_ (funcall iter)))
             (if (iterend? _) _ (cons _ (n-of (funcall iter) n)))))))

(defun force (iterator)
  (for x in iterator collect x))

(defun vforce (iterator)
  (with-vcollector collect
    (for item in iterator do (collect item))))

(defun sforce (iterator)
  (with-char-collector collect
    (for item in iterator do (collect item))))

(defun itermap (fn iterator &rest iterators)
  (setf iterator (iterator iterator))
  (if iterators
    (bb iterators (mapcar 'iterator iterators)
        (fn ()
          (bb _ (funcall iterator)
              (if (iterend? _) _ (apply fn _ (mapcar 'funcall iterators))))))
    (fn ()
      (bb _ (funcall iterator)
          (if (iterend? _) _ (funcall fn _))))))

(defun itercat (&rest iterators)
  (let ((current-iterator (iterator (pop iterators))))
    (fn ()
      (if (null iterators)
        (funcall current-iterator)
        (iterate loop1 ()
          (bb _ (funcall current-iterator)
              (if (eq _ (iterend))
                (progn (setf current-iterator (iterator (pop iterators))) (loop1))
                _)))))))

(defun iterslice (iterator start &optional end step)
  (dotimes (i start) (funcall iterator))
  (if end
    (if step
      (counter start end step (fn (_)
                                (dotimes (i (1- step)) (funcall iterator))
                                (funcall iterator)))
      (counter start end 1 (fn (_) (funcall iterator))))
    iterator))

(defun iter-reduce (fn iterator &optional (initial-value nil initial-value-supplied?))
  (unless initial-value-supplied? (setf initial-value (funcall iterator)))
  (for value in iterator do (setf initial-value (funcall fn initial-value value)))
  initial-value)

(define-method (slice (iter function) start &optional end step)
  (iterslice iter start end step))

(define-method (cat (v vector) &rest vectors)
  (apply 'concatenate 'vector v vectors))

(define-method (cat (s string) &rest strings)
  (apply 'concatenate 'string s strings))

(define-method (cat (l list) &rest lists)
  (apply 'concatenate 'list l lists))

(define-method (cat (f function) &rest things)
  (apply 'itercat f (mapcar 'iterator things)))

; GMAP - Generalized MAP, works on everything including iterators
(define-method (gmap fn (l list) &rest lists) (apply 'mapcar fn l lists))

(define-method (gmap fn (v vector) &rest vectors) (apply 'cl:map 'vector fn v vectors))

(define-method (gmap fn (f function) &rest functions) (apply 'itermap fn f functions))

(defun leaves (tree)
  (fn ()
    (iterate loop1 ()
      (mcond (null tree) (iterend)
             (atom tree) (prog1 tree (setf tree nil))
             (atom (car tree)) (pop tree)
             t (bb tr (pop tree)
                   (aif (cdr tr) (push it tree))
                   (push (car tr) tree)
                   (loop1))))))

#|
Examples:

(for (elt cnt) in (zip '(a b c) (counter)) collect (list elt cnt))
(for c in "abc" do (print c))
(for l in (lines "abc
def
ghi") do (print l))
|#
