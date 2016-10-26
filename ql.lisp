
(defun this-directory (&optional (path-extension ""))
  (merge-pathnames path-extension
                   (directory-namestring
                    (or *load-pathname* *default-pathname-defaults*))))

(defvar *quicklisp-path* (merge-pathnames "quicklisp/" (this-directory)))

(defun setup-quicklisp ()
  (let ((*load-verbose* t))
    (if (probe-file (merge-pathnames "setup.lisp" *quicklisp-path*))
      (load (merge-pathnames "setup.lisp" *quicklisp-path*))
      (progn
        (format t "~&Installing Quicklisp...")
        (ensure-directories-exist *quicklisp-path*)
        #+CCL (load "http://beta.quicklisp.org/quicklisp.lisp")
        #-CCL (load (merge-pathnames "quicklisp" (this-directory)))
        (funcall (intern "INSTALL" :quicklisp-quickstart) :path *quicklisp-path*)))))

(setup-quicklisp)

(defun module-provide-quicklisp (&rest args)
  (ignore-errors (apply 'ql:quickload args)))

(pushnew 'module-provide-quicklisp *module-provider-functions*)
