
(require :named-readtables)
(use-package :named-readtables)
(hi::defindent "defreadtable" 1)
(defreadtable ccl (:merge :current)) ; Danger Will Robinson!

(let ( (r (find-readtable nil)) )
  (defun read-symbol (stream)
    (let ( (*readtable* r) )
      (read-preserving-whitespace stream))))

(defun symbol-reader-macro-reader (stream char)
  (unread-char char stream)
  (let ( (s (read-symbol stream)) )
    (if (not (symbolp s)) ; Might not be a symbol if *read-base* is > 10
      (return-from symbol-reader-macro-reader s))
    ; Experimental, should probably be a hook function
    (if (not (fboundp s))
      (setf (symbol-function s)
            (lambda (&rest args)
              (warn "~A is not defined as a function, using value instead." s)
              (apply (symbol-value s) args))))
    (let ( (f (get s 'symbol-reader-macro)) )
      (if f (funcall f stream s) s))))

(defreadtable symbol-reader-macros (:merge ccl))

(let ((r (find-readtable 'symbol-reader-macros)))
  (map nil (lambda (c) (set-macro-character c 'symbol-reader-macro-reader t r))
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun set-macro-symbol (symbol readfn)
  (setf (get symbol 'symbol-reader-macro) readfn)
  t)

(defun make-special-form-reader (nargs)
  (lambda (stream symbol)
    (cons symbol (loop for i from 1 upto nargs
                   collect (if (listen stream) (read stream))))))


; Prefix funcall syntax, i.e. f(x).  Requires no space between function
; name and open paren

(defvar $prefix-funcall-syntax-arg-reader 'read)

(defun prefix-funcall-syntax-reader (stream char)
  (unread-char char stream)
  (let ( (s (read-symbol stream)) )
    (if (and
         ; Disable for #+foo(baz) -- the FOO part reads as a keyword
         (symbolp s) ; Might not be a symbol if *read-base* is > 10
         (not (eql (symbol-package s) (find-package :keyword)))
         (eql (peek-char nil stream nil nil) #\())
      (cons s (funcall $prefix-funcall-syntax-arg-reader stream))
      s)))

(defreadtable prefix-calls (:merge ccl))

(let ((r (find-readtable 'prefix-calls)))
  (map nil (lambda (c) (set-macro-character c 'prefix-funcall-syntax-reader t r))
       "abcdefghijklmnopqrstuvwxyz"))

; Fix various reader macros which are broken by the above:

(let ((rt1 (find-readtable 'ccl)))
  (defun fix-dispatch-reader-macro (c1 c2 rt2)
    (let* ((s (intern (format nil "original-~A~A-reader" c1 c2))))
      (unless (fboundp s)
        (setf (symbol-function s) (get-dispatch-macro-character c1 c2 rt1)))
      (set-dispatch-macro-character c1 c2
        (lambda (stream c1 c2)
          (let ((*readtable* rt1))
            (funcall s stream c1 c2)))
        rt2)))
  (defun fix-dispatch-reader-macros (rt)
    (dolist (c '(#\_ #\: #\/ #\$))
      (fix-dispatch-reader-macro #\# c rt))))

(fix-dispatch-reader-macros (find-readtable 'symbol-reader-macros))
(fix-dispatch-reader-macros (find-readtable 'prefix-calls))


#|
Examples:

(defreadtable srm-test (:fuze symbol-reader-macros prefix-calls))

(in-readtable srm-test)

cos(1)

(set-macro-symbol 'setf (make-special-form-reader 2))

Setf x 1

(require :infix) ; Anti-social: installs a global #i reader macro

(setf $prefix-funcall-syntax-arg-reader
  (lambda (stream) (list (infix::infix-reader stream nil nil))))

cos(1+2)

(require :parcil)

(setf $prefix-funcall-syntax-arg-reader 'parcil-read)

cos(1+2)

|#
