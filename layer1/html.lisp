
(require :ergolib)
(require :cl-who)
(unintern 'fmt)
(use-package :cl-who)

(defun fmt (s &rest args)
  (without-length-restrictions
   (apply 'format nil s args)))

(defmacro htm (&rest body)
  `(with-html-output (*standard-output*) ,@body))
