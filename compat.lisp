
#+hemlock
(import 'hi::defindent)

#-hemlock
(defmacro defindent (name indent) nil)

#+CCL (shadowing-import '(ccl::while ccl::until))

#+CLISP
(without-package-lock (cl-user)
  (shadow '(require probe-file with-gensyms)))
#+CLISP
(progn
  (use-package :clos)
  (defvar *module-provider-functions* nil)

  (defun require (module &optional path)
    (if path (return-from require (cl:require module path)))
    (dolist (f *module-provider-functions*)
      (if (funcall f module)
	  (progn (provide module) (return-from require module))))
    (error "Unable to load module ~A" module))

  (defun probe-file (path)
    (or (ignore-errors (truename path))
	(ignore-errors
	  (truename (concatenate 'string (namestring path) "/")))))
)

#+SBCL
(progn
  (use-package :sb-mop)
  (defun arglist (thing)
    (if (symbolp thing) (setf thing (symbol-function thing)))
    (if (typep thing 'standard-generic-function)
	(SB-PCL::GF-LAMBDA-LIST thing)
	(SB-KERNEL:%SIMPLE-FUN-ARGLIST thing)))
)

#+ABCL (use-package :mop)
#+ABCL (import 'system::*module-provider-functions*)
#+ABCL (shadow 'collect)

#-CCL(progn

(shadow 'set)

(defmacro while (condition &rest body)
  `(loop while ,condition do (progn ,@body)))

(defmacro while (condition &body body)
  `(loop while ,condition do (progn ,@body)))

(defmacro until (condition &body body)
  `(loop until ,condition do (progn ,@body)))

(defvar *MODULE-SEARCH-PATH* nil)

; NOTE: There is a bug in SBCLs pathname parser.  ".lisp" parses as
; a path whose name is ".lisp" rather than one whose type is "lisp".

(defvar *.lisp-pathname* "_.lisp")
(defvar *.fasl-pathname* "_.fasl")

(defun find-module-pathnames (module)
  "Returns the file or list of files making up the module"
  (let ((mod-path (make-pathname :name (string-downcase module) :defaults "")))
    (dolist (path-cand *module-search-path* nil)
      (let* ((base (merge-pathnames mod-path path-cand))
	     (basefile (probe-file base))
	     (lispfile (probe-file (merge-pathnames base *.lisp-pathname*)))
	     (faslfile (probe-file (merge-pathnames base *.fasl-pathname*)))
	     (file (if (and lispfile faslfile)
		       (if (> (file-write-date faslfile) (file-write-date lispfile))
			   faslfile lispfile)
		       (or basefile lispfile faslfile))))
	(if file (return file))))))

(defun module-provide-search-path (module)
  ;; (format *debug-io* "trying module-provide-search-path~%")
  (let* ((module-name (string module))
         (pathname (find-module-pathnames module-name)))
    (when pathname
      (if (consp pathname)
        (dolist (path pathname) (load path))
        (load pathname))
      (provide module))))

(pushnew 'module-provide-search-path *module-provider-functions*)

(defvar *print-string-length* nil)

)
