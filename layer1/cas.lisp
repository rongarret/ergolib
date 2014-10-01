
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CAS - Content Addressable Store
;;;

(require :ergolib)
(require :hashlib)

(defun vector->int (v &optional (big-endian nil))
  (bb v (if big-endian v (reverse v))
      n 0
      (loop for m across v do (setf n (+ m (ash n 8))))
      n))

(define-method (stdhash (s string))
  (b32 (vector->int (sha1 s) t)))

(define-method (stdhash (v vector))
  (b32 (vector->int (sha1 v) t)))

#|
; Slow reference implementation
(define-method (stdhash (p pathname))
  (b32 (vector->int (sha1 (file-contents p :binary)) t)))
|#

; This is much (10x) faster for large files
(define-method (stdhash (p pathname))
  (b32 (parse-integer (system (list "shasum" (namestring (truename p))))
                      :radix 16 :junk-allowed t)))

(define-method (stdhash (l list))
  (stdhash (->string l)))

;;; Content-addressable store (for large blobs)

(define-class content-addressable-store path)

(define-print-method (content-addressable-store path)
  "#<Content-addressable store at ~A>" path)

(defun make-cas (path)
  (make <content-addressable-store> :path (directory-namestring path)))

(define-method (filepath (cas content-addressable-store path) hash)
  (make-pathname :directory path :name hash))

(define-method (ref1 (cas content-addressable-store path) hash)
  (bb path1 (filepath cas hash)
      (if (probe-file path1) (return (file-contents path1 :binary)))
      files (directory (filepath cas (strcat hash "*")))
      n (length files)
      (if (= n 1) (return (file-contents (1st files) :binary)))
      (if (= n 0) (error "CAS hash not found: ~A" hash))
      (error "CAS hash ~A is ambiguous" hash)))

(define-method (del (cas content-addressable-store path) hash)
  (delete-file (make-pathname :directory path :name hash)))

(define-method (cas-store (cas content-addressable-store path) (s string))
  (cas-store cas (string-to-bytes s)))

(defun write-bytes (v path)
  (if (probe-file path) (delete-file path))
  (with-open-file (f path :direction :output :if-does-not-exist :create :element-type 'u8)
    (write-sequence v f)))

(define-method (cas-store (cas content-addressable-store path) (v vector))
  (bb hash (stdhash v)
      path (filepath cas hash)
      (if (probe-file path)
        (if (equalp (file-contents path :binary) v)
          hash
          (bb path (filepath cas (strcat hash "-X"))
              (write-bytes v path)
              (error "Hash ~A exists, but file contents don't match" hash)))
        (write-bytes v path))
      hash))

(define-method (keys (cas content-addressable-store path))
  (mapcar 'pathname-name (directory (merge-pathnames "*" path))))

(define-method (verify (cas content-addressable-store))
  (for k in (keys cas) do
    (if (not (equalp (stdhash (ref cas k)) k))
      (format t "~&*** CAS content mismatch: ~A" k)))
  t)

(defv $cas nil)

(define-method (cas-store (n null) thing)
  (error "$CAS is not set"))

(define-method (store (s string)) (cas-store $cas s))

(define-method (store (v vector)) (cas-store $cas v))

(defun vector->string (v) (decode-string-from-octets v :external-format :utf-8))

(defmethod unstore ((s (eql 'string)) id) (vector->string (ref $cas id)))

(defmethod unstore ((s (eql 'vector)) id) (ref $cas id))

(defun serialize (thing)
  (without-length-restrictions
   (let ((*print-readably* t))
     (prin1-to-string thing))))

(defun deserialize (s) (safely (read-from-string s)))

(define-method (store (l list)) (cas-store $cas (serialize l)))

(defmethod unstore ((s (eql 'list)) id) (deserialize (unstore 'string id)))
