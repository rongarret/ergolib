
(cl:defpackage :ergo (:use :cl #+CCL :ccl) (:export :ergo :cl))
(cl:in-package :ergo)

(import '(cl-user::while cl-user::until cl-user::defindent))

(defvar *default-package* "CL-USER")

(defmacro set-global (var value)
  `(progn
     #+CCL(CCL::DEF-STANDARD-INITIAL-BINDING ,var ,value)
     #+CCL(ccl::%set-sym-global-value ',var ,value)
     (setf ,var ,value)))

#+HEMLOCK
(defun set-default-package-for-buffer (buffer)
  (hi::defhvar "Default Package" "" :buffer buffer)
  (setf (hi::variable-value 'hemlock::default-package :buffer buffer) *default-package*))

#+HEMLOCK
(pushnew #'set-default-package-for-buffer (hi::variable-value 'HEMLOCK::MAKE-BUFFER-HOOK))

(defun set-default-package (name)
  (setf *default-package* name)
  (let ((pkg (or (find-package name) (make-package name))))
    (set-global *package* pkg)
    #+HEMLOCK
    (dolist (b (hemlock::all-buffers))
      (set-default-package-for-buffer b))
    #+HEMLOCK
    (gui::eval-in-listener (format nil "(in-package ~S)" *default-package*))
    name))

(defun ergo ()
  (set-default-package "ERGO"))

(defun cl ()
  (set-default-package "CL-USER"))

(use-package :ergo :cl-user)

(ergo)
