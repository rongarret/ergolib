
(require :sql)
(require :hashlib)

(defun vector->int (v &optional (big-endian nil))
  (bb v (if big-endian v (reverse v))
      n 0
      (loop for m across v do (setf n (+ m (ash n 8))))
      n))

(define-method (stdhash (s string))
  (b32 (vector->int (sha1 s) t)))

#|
; Slow reference implementation
(define-method (stdhash (p pathname))
  (b32 (vector->int (sha1 (file-contents p :latin1)) t)))
|#

; This is much faster
(define-method (stdhash (p pathname))
  (b32 (parse-integer (system (format nil "shasum ~A" (truename p)))
                      :radix 16 :junk-allowed t)))

(define-method (stdhash (l list)) (stdhash (prin1-to-string l)))

;;; Content-addressable store (for large blobs)

(define-class content-addressable-store path)

(defun make-cas (path)
  (make-content-addressable-store :path (directory-namestring path)))

(define-method (ref1 (cas content-addressable-store path) hash)
  (bb path1 (make-pathname :directory path :name hash)
      (if (probe-file path1) (return (file-contents path1 :binary)))
      files (directory (make-pathname :directory path :name (strcat hash "*")))
      n (length files)
      (if (= n 1) (return (file-contents (1st files) :binary)))
      (if (= n 0) (error "CAS hash not found: ~A" hash))
      (error "CAS hash ~A is ambiguous" hash)))

(define-method (del (cas content-addressable-store path) hash)
  (delete-file (make-pathname :directory path :name hash)))

(define-method (cas-store (cas content-addressable-store path) (s string))
  (bb hash (stdhash s)
      path (make-pathname :directory path :name hash)
      (if (probe-file path)
        (if (string= (file-contents path) s)
          hash
          (error "Hash ~A exists, but file contents don't match" hash))
        (with-open-file (f path :direction :output)
          (write-string s f)))
      hash))

(define-method (cas-store (cas content-addressable-store path) (v vector))
  (bb s (bytes-to-string v :latin1)
      hash (stdhash s)
      path (make-pathname :directory path :name hash)
      (if (probe-file path)
        (if (equalp (file-contents path :binary) v)
          hash
          (error "Hash ~A exists, but file contents don't match" hash))
        (with-open-file (f path :direction :output :element-type 'u8)
          (write-sequence v f)))
      hash))

(define-method (keys (cas content-addressable-store path))
  (mapcar 'pathname-name (directory (merge-pathnames "*" path))))

(define-method (store (s string)) (cas-store $cas s))

(define-method (store (v vector)) (cas-store $cas v))

(defun vector->string (v) (decode-string-from-octets v :external-format :utf-8))

(defmethod unstore ((s (eql 'string)) id) (vector->string (ref $cas id)))

(defmethod unstore ((s (eql 'vector)) id) (ref $cas id))

(define-method (serialize (l list)) (prin1-to-string l))

(defun deserialize (s) (safely (read-from-string s)))

(define-method (store (l list)) (cas-store $cas (serialize l)))

(defmethod unstore ((s (eql 'list)) id) (deserialize (unstore 'string id)))

#|
; Convergent encryption, needs work to handle binary data properly

(defun convergent-encrypt (s)
  (bb k (stdhash s)
      (values (aes256-encrypt s k) k)))

(defun convergent-decrypt (s k)
  (bb :mv (cleartext status) (aes256-decrypt s k)
      (if (zerop status) cleartext nil)))

(defun random-string (n)
  (map 'string 'code-char (n-of (random 256) n)))

(defun convergent-encryption-test (&optional (n 100))
  (bb s (random-string n)
      s1 (multiple-value-call 'convergent-decrypt (convergent-encrypt s))
      (and s1 (string= s s1))))

(define-method (ce-cas-store (cas content-addressable-store path) s1)
  (bb hash1 (stdhash s1)
      s2 (convergent-encrypt s1)
      hash2 (stdhash s2)
      path (make-pathname :directory path :name hash2)
      (if (probe-file path)
        (if (string= (file-contents path) s2)
          (values hash1 hash2)
          (error "Hash ~A exists, but file contents don't match" hash2))
        (with-open-file (f path :direction :output)
          (princ s2 f)))
      (values hash1 hash2)))
|#

;;; Persistent dictionaries

(defun setup-db-for-persistent-dictionaries (db)
  (query db "create table if not exists _id (id integer primary key autoincrement)")
  (query db "create table if not exists _dictionary
  (id integer, key, value, primary key(id, key))"))

(let ((lock (make-lock)))
  (defun generate-unique-id (db)
    (with-lock-grabbed (lock)
      (sqlite:with-transaction (database-db db)
        (query db "insert into _id default values")
        (last-inserted-rowid db)))))

(define-class persistent-dictionary (cache (->)) id db)

(define-print-method (persistent-dictionary db id cache)
                     "#<Persistent dictionary ~A with ~A member(s)>"
  id (size cache))

(define-method (iterator (pd persistent-dictionary cache)) (iterator cache))

(defun make-persistent-dictionary (db &optional id)
  (bb id (or id (generate-unique-id db))
      d (make-instance 'persistent-dictionary :id id :db db)
      (reload d)))

(define-method (reload (d persistent-dictionary id db cache))
  (setf cache (->))
  (for kv in (query db "select key,value from _dictionary where id=~A" id)
    (setf (ref cache (sql-unrender (1st kv))) (sql-unrender (2nd kv))))
  d)

(define-method (ref1 (d persistent-dictionary cache) key) (ref cache key))

(define-method (has-key (d persistent-dictionary cache) key)
  (has-key cache key))

(define-method (refd (d persistent-dictionary) key default)
  (if (has-key d key) (ref1 d key) default))

(define-method (del (d persistent-dictionary cache id db) key)
  (query db "delete from _dictionary where id=~A and key=~A"
         id (sql-escape (sql-render key)))
  (del cache key))

(define-method (setref (d persistent-dictionary id db cache) key value)
  (query db "insert or replace into _dictionary(id, key, value)
  values(~A, ~A, ~A)" id (sql-escape (sql-render key)) (sql-escape (sql-render value)))
  (setf (ref cache key) value))

(define-method (keys (d persistent-dictionary cache)) (keys cache))

(define-method (primary-key (d persistent-dictionary id)) id) ; Deprecated
(define-method (id (d persistent-dictionary id)) id)

;;; Sets and persistent sets

(define-class (set dictionary))

(define-print-method (set implementation) "#<Set with ~A member(s)>" (size implementation))

(define-method (members (s set)) (keys s))

(define-method (member? (s set) thing)
  (ref s thing))

(define-method (add (s set) thing)
  (setf (ref s thing) t))

(define-class (persistent-set set))

(define-method (primary-key (s persistent-set implementation)) ; Deprecated
  (primary-key implementation))

(define-method (id (s persistent-set implementation)) (id implementation))

(defmethod unstore ((s (eql 'persistent-set)) id)
  (make-persistent-set $db id)) ; FIXME: Should have a cache

(defmethod unstore ((s (eql 'persistent-dictionary)) id)
  (make-persistent-dictionary $db id)) ; FIXME: Should have a cache

(define-print-method (persistent-set implementation) "#<Persistent set ~A with ~A member(s)>"
  (primary-key implementation) (size implementation))

(defun make-persistent-set (db &optional id)
  (make-instance 'persistent-set :implementation (make-persistent-dictionary db id)))

(define-method (reload (ps persistent-set implementation))
  (reload implementation)
  ps)

(define-method (delall thing)
  (for k in (keys thing) do (del thing k))
  thing)

(define-method (ref1 (n null) k) (declare (ignore k)) nil)
(define-method (member? (n null) k) (declare (ignore k)) nil)

;;; Connections

(defv $connections (->))

(defun connect (o1 o2)
  (add (ref (ref $connections o1 (->)) (type-of o2) (make-set)) o2))

(defun connect2 (o1 o2)
  (connect o1 o2)
  (connect o2 o1))

(defun disconnect (o1 o2)
  (del (ref (ref $connections o1) (type-of o2)) o2))

(defun disconnect-all (object class)
  (delall (ref (ref $connections object) class)))

(defun disconnect2 (o1 o2)
  (disconnect o1 o2)
  (disconnect o2 o1))

(defun connections (object class)
  (members (ref (ref $connections object) class)))

(defun connected? (o1 o2)
  (member? (ref (ref $connections o1) (type-of o2)) o2))


#|

(setf u (make-user :name "Bill"))
(setf u (ref (find-class 'user) 1))
(setf s (make-persistent-set $db 1))
(delall s)
(add s 123)
(add s 1.23)
(add s 4.56d0)
(add s "string")
(add s 'symbol)
(add s :keyword)
(add s '(list of 4 "things"))
(add s u)

(define-class foo)
(define-class baz)
(setf f1 (make-foo) f2 (make-foo) b1 (make-baz) b2 (make-baz))

(connect f1 b1)
(connected? f1 b1)
(connected? b1 f1)


;;; Persistent connections

(define-class connection _id o1 o2)
(make-persistent 'connection $db)

(define-class foo _id)
(make-persistent 'foo $db)

(define-class baz _id)
(make-persistent 'baz $db)

(defv index1 (index (ref $db 'connection) 'o1))
(defv index2 (index (ref $db 'connection) 'o2))
(defun connect (o1 o2)
  (store (make-connection :o1 o1 :o2 o2)))
(connect f1 b1)

;;; Referents

(require :persistence)

(define-class referent class id)

(defun make-referent (thing)
  (bb id (or (slot-value thing (primary-key (find-class (type-of thing))))
             (store thing))
      (make-instance 'referent :class (type-of thing) :id id)))

(define-print-method (referent class id) "#<Reference to ~A ~A>" class id)

(define-method (deref (r referent class id))
  (ref (find-class class) id))

(define-modify-macro deref! () deref)

; So many options!
(define-method (deref!! (r referent class))
  (bb o (deref r)
      (change-class r class)
      (for k in (keys o) do (setf (slot-value r k) (slot-value o k)))
      r))
|#
