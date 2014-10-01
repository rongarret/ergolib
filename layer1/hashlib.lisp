
; Requires libssl-dev

(require :ergolib)
(require :rffi)
(require :cl+ssl)

(deftype u8 () '(unsigned-byte 8))

(define-method (hash (v vector) hashfn hashsize)
  (bb
   (for b in v do (assert (typep b 'u8)))
   s (bytes-to-string v :latin1)
   :mv (v p) (make-heap-ivector hashsize 'u8)
   (funcall hashfn s (length s) p)
   (prog1 (copy-seq v) (dispose-heap-ivector v))))

(define-method (hash (s string) hashfn hashsize)
  (hash (string-to-bytes s :utf-8) hashfn hashsize))

(defmacro defhash (name size)
  `(progn
     (defff (,(symbol-name name) ,(symcat '_ name)) (:cstr :int :ptr) :ptr)
     (define-method (,name (v vector)) (hash v ',(symcat '_ name) ,size))
     (define-method (,name (s string)) (hash s ',(symcat '_ name) ,size))
     ',name))

; NOTE: only SHA1 is actually used at the moment
(defhash md5 16)
(defhash sha1 20)
(defhash sha256 32)
(defhash sha512 64)
