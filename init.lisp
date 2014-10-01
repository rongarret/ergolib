
(defun this-directory (&optional (path-extension ""))
  ; *loading-file-source-file* ?
  (merge-pathnames path-extension (directory-namestring *load-pathname*)))

(defun addpath (path)
  (setf path (probe-file (this-directory path)))
  (unless path (error "Directory not found: ~A" path))
  (pushnew path *module-search-path* :test 'equalp))

(addpath (this-directory))
(addpath (this-directory "core"))
(addpath (this-directory "layer1"))
(addpath (this-directory "web"))
(addpath (this-directory "experimental"))
