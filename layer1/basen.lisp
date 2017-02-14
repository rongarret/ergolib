
(require :ergolib)

(defv $b58-chars "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

(define-method (basen (i integer) base)
  (reverse
   (with-char-collector collect
     (while (> i 0)
       (collect (ref $b58-chars (mod i base)))
       (setf i (truncate i base))))))

; FIXME: Stack overflow on NIL
(define-method (basen (seq sequence) base)
  (bb p (or (position 0 seq :test-not '=) (length seq))
      (strcat (coerce (n-of (ref $b58-chars 0) p) 'string)
              (basen (octets->integer seq) base))))

(defun unbasen (s base)
  (unless s (return-from unbasen nil))
  (bb p (position (ref $b58-chars 0) s :test-not 'eql)
      (if (null p) (return (coerce (n-of 0 (length s)) 'octets)))
      n 0
      (for c in s do (setf n (+ (* n base) (position c $b58-chars))))
      (concatenate 'octets (n-of 0 p) (integer->octets n))))

(defun b58 (v) (basen v 58))
(defun unb58 (s) (unbasen s 58))

(require :cl-base64)
(defun b64 (v) (CL-BASE64:USB8-ARRAY-TO-BASE64-STRING (coerce v 'octets)))
(defun unb64 (s) (and s (CL-BASE64:BASE64-STRING-TO-USB8-ARRAY s)))

(defun unhex-to-bytes (s)
  (dlet (($b58-chars "0123456789ABCDEF"))
    (unbasen s 16)))

(defun unhex-to-bignum (s)
  (parse-integer s :radix 16))
