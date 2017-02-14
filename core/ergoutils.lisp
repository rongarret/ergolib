(require :binding-block)
(require :ergoclos)
(require :iterators)

(defun read-all (stream)
  (if (eq (stream-element-type stream) 'character)
    (with-char-collector collect (for c in stream do (collect c)))
    (with-vcollector collect (for b in stream do (collect b)))))

(deftype u8 () '(unsigned-byte 8))
(deftype octet () 'u8)
(deftype octets () '(vector u8))

(defun octets (&rest octets) (coerce octets 'octets))


(defun octets->integer (octets &optional (little-endian nil))
  (if little-endian (setf octets (reverse octets)))
  (and octets (bb n 0 (for i in octets do (setf n (+ i (ash n 8)))) n)))

(defun integer->octets (n &optional (little-endian nil))
  (mcond (< n 0) (error "Cannot convert negative numbers to bytes")
         (= n 0) (octets 0)
         (bb l nil
             (while (> n 0)
               (push (logand n #xFF) l)
               (setf n (ash n -8)))
             (if little-endian (setf l (reverse l)))
             (coerce l 'octets))))

(defun octets->string (v &optional (encoding :utf-8))
  (decode-string-from-octets (coerce v 'octets) :external-format encoding))

(defun string->octets (s &optional (encoding :utf-8))
  (encode-string-to-octets s :external-format encoding))

(defv $file-path (list "~" "~/Desktop/"))

(defun find-file (path)
  (or (probe-file path)
      (some (fn (p1) (probe-file (merge-pathnames path p1))) $file-path)
      (probe-file (merge-pathnames path (this-directory)))
      (probe-file (merge-pathnames path (current-directory)))
      (error "File ~A not found" path)))

(defmacro with-file ((var path &rest args) &body body)
  `(with-open-file (,var (find-file ,path) ,. args) ,.body))

(defun file-contents (path &optional (encoding :default))
  (if (eq encoding :binary)
    (with-file (f (pathname path) :element-type 'u8)
      (let ((result (make-array (file-length f) :element-type 'u8)))
        (read-sequence result f)
        result))
    (with-file (f (pathname path) :external-format encoding)
      (read-all f))))

(defun set-file-contents (path thing)
  (with-open-file (f path :direction :output :if-exists :supersede :if-does-not-exist :create
                     :element-type (if (typep thing 'octets) 'u8 'character))
    (if (or (stringp thing) (typep thing 'octets))
      (write-sequence thing f)
      (progn (princ ";;; SFC-data" f) (printl thing f)))))

(defsetf file-contents set-file-contents)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Unix utils
;;;
#+CCL ; Need to replace with something more portable
(defun system (cmd &optional stdin (external-format :utf-8))
  (if (typep stdin 'string) (setf stdin (make-string-input-stream stdin)))
  (if (typep cmd 'string) (setf cmd (split cmd #\Space)))
  (bb p (run-program (fst cmd) (rst cmd) :output :stream :input stdin :wait nil
                     :external-format external-format)
      s (read-all (external-process-output-stream p))
      (when (ccl::external-process-pid p)
        (with-interrupts-enabled
            (wait-on-semaphore (ccl::external-process-completed p))))
      (values s (nth-value 1 (external-process-status p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Slice
;;;
(define-method (slice (s sequence) start &optional end step)
  (bb len (length s)
      start (if (< start 0) (max (+ len start) 0) (min start len))
      end (if (null end) len (min len (if (< end 0) (max (+ len end) start) end)))
      s (subseq s start end)
      (if (null step) (return s))
      cnt (/ len step)
      s1 (subseq s 0 cnt)
      (for i in (counter 0 cnt) do (setf (ref s1 i) (ref s (* i step))))
      s1))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Split and join
;;;
(define-method (split (l list) elt &key (test 'eql) (key 'identity) (max -1))
  (iterate loop1 ( (l l) (result '()) (result1 '()) (max max) )
    (cond ( (null l) (reverse (cons (reverse result1) result)) )
	  ( (zerop max) (reverse (cons l result)) )
	  ( (funcall test elt (funcall key (fst l)))
	    (loop1 (rst l) (cons (reverse result1) result) '() (1- max)) )
	  (t (loop1 (rst l) result (cons (fst l) result1) max)))))

(define-method (split (v vector) elt &key (test 'eql) (key 'identity) (max -1))
  (with-collector collect
    (do* ( (i 0 (1+ j))
           (j (position elt v :test test :key key)
	      (position elt v :test test :key key :start i))
	   (max max (1- max)) )
	 ( (or (null j) (zerop max)) (collect (subseq v i)) )
      (collect (subseq v i j)))))

(define-method (split (s1 string) (s2 string) &key test key (max -1))
  (declare (ignore test key))
  (with-collector collect
    (do* ( (i 0 (+ j (length s2)))
	   (j (search s2 s1) (search s2 s1 :start2 i))
	   (max max (1- max)) )
	 ( (or (null j) (zerop max)) (collect (subseq s1 i)) )
      (collect (subseq s1 i j)))))

(defun two-way-split (s c1 c2)
  (for s in (split s c1) collect (split s c2)))

(defun join (strings &optional (delim ""))
  (with-char-collector collect
    (if strings (collect (first strings)))
    (dolist (s (rst strings))
      (collect delim)
      (collect s))))

(defun strsubst (s1 s2 s3) (join (split s1 s2) s3))

(defun string-prefix-equal (s1 s2)
  (and (stringp s1) (stringp s2)
       (let ( (i (min (length s1) (length s2))) )
         (string-equal s1 s2 :end1 i :end2 i))))

(defun starts-with (s1 s2)
  (and (>= (length s1) (length s2)) (equal (slice s1 0 (length s2)) s2)))

(defun ends-with (s1 s2)
  (and (>= (length s1) (length s2)) (equal (slice s1 (- (length s2))) s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Date and time
;;;

(defun now () (get-universal-time))

(defc long-month-names #(january february march april may june july
                                 august september october november december))
(defc short-month-names #(jan feb mar apr may jun jul aug sep oct nov dec))

(defmacro with-decoded-universal-time (ut &body body)
  `(mvbind (s m h d mo y) (decode-universal-time ,ut)
     (declare (ignorable s m h d mo y))
     ,@body))

(defun format-date-time (&optional (ut (now)))
  (with-decoded-universal-time ut
    (format nil "~d/~d/~d ~d:~2,'0d:~2,'0d" mo d y h m s)))

(defun format-date (&optional (ut (now)))
  (with-decoded-universal-time ut
    (format nil "~d ~a ~d" d (elt short-month-names (1- mo)) y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Spawn
;;;
(defmacro spawn (&rest body)
  (let ((name (mcond (stringp (car body)) (pop body)
                     (eq (car body) :name) (progn (pop body) (pop body))
                     t '(symbol-name (gensym "SPAWNED-TASK"))))
        (namevar (gensym "NAME"))
        (prf (or #+EASYGUI 'gui:background-process-run-function 'process-run-function)))
    `(let ((,namevar ,name))
       (prog1
           (,prf (list :name (princ-to-string ,namevar)) #+LISPWORKS nil (fn () ,@body))
         #+EASYGUI (sleep 0.1) ; Workaround for race condition in gui:background-process-run-function
         ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter
;;;
(define-method (filter (l list) function)
  "Similar to jQuery.grep - returns a list of items
  where function returns true for that item"
  (with-collector collect
    (for item in l do
      (if (funcall function item)
        (collect item)))))

(define-method (filter (v vector) function)
  (with-vcollector collect
    (for item in v do
      (if (funcall function item)
        (collect item)))))

(define-method (filter (s string) function)
  (with-char-collector collect
    (for c in s do
      (if (funcall function c)
        (collect c)))))

(define-method (filter thing function)
  (with-collector collect
    (for item in thing do
      (if (funcall function item) (collect item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRY
;;;
(defmacro try (form &rest stuff)
  (mcond
   ; (try (form)) - equivalent to (ignore-errors (form))
   (null stuff) `(ignore-errors ,form)
   ; (try (form) var [(handler-form) ...]) - bind condition to VAR
   (symbolp (car stuff)) (with-gensyms (value condition)
                           `(bb :mv (,value ,condition) (ignore-errors ,form)
                                (if ,condition
                                  ,(if (rst stuff)
                                     `(bb ,(car stuff) ,condition (progn ,@(rst stuff)))
                                     condition)
                                  ,value)))
   ; (try (form) (handler-form) ...) - execute handler forms without binding the condition
   (with-gensyms (value condition)
     `(bb :mv (,value ,condition) (ignore-errors ,form)
          (if ,condition (progn ,@stuff) ,value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ONCE-ONLY
;;;
(defv $once-only-caches nil)

(defmacro once-only (form)
  (with-gensym val
    (push val $once-only-caches)
    `(if (boundp ',val) (symbol-value ',val) (set ',val ,form))))

(defun reset-once-only-caches ()
  (for s in $once-only-caches do (makunbound s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONTAINTS-DUPLICATES?  Like (equal seq (remove-dupplicates seq)) but faster
;;;
(defun contains-duplicates? (l &key (test 'equal))
  (bb h (make-hash-table :test test)
      (for e in l do
        (if (gethash e h)
          (return-from contains-duplicates? (values t e))
          (setf (gethash e h) t)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Debugging utilities
;;;
(defmacro name (thing &optional name)
  (bb val (eval thing)
      class (class-name (class-of val))
      `(defmethod print-object ((x (eql ',val)) stream)
         (format stream "#<~A ~A>" ',class ',(or name thing)))))

(defun unname (thing)
  (let ((m (find-method #'print-object () `((eql ,thing) t))))
    (if m (remove-method #'print-object m))))

(defun logstream ()
  #+HEMLOCK (HEMLOCK-EXT:TOP-LISTENER-OUTPUT-STREAM)
  #-HEMLOCK *terminal-io*)

(defun logmsg (s &rest args)
  (let ((logstream (logstream)))
    (format logstream "~&[~A] " (format-date-time (now)))
    (if (stringp s)
      (apply 'format logstream s args)
      (if args
        (princ (cons s args) logstream)
        (princ s logstream)))
    (terpri logstream)
    (force-output logstream))
  (values))

#+CCL ; Need to replace with trivial-backtrace
(defun get-backtrace ()
  (with-output-to-string (s)
    (ccl:print-call-history :detailed-p nil :stream s)))
