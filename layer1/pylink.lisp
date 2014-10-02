
(require :ergolib)

(define-class python-server process instream outstream prompt)

(defv $default-python-prompt "πλπλπ") ; Some string we never expect to see in our output

(defun make-python-server (&key (path "python") (prompt $default-python-prompt))
  (bb process (run-program path (list "-i")
                           :sharing :lock ; For now
                           :input :stream :output :stream :wait nil
                           :external-format :utf-8
                           :env '(("PYTHONIOENCODING" . "utf-8")))
      pyserver (make-instance 'python-server :process process
                 :instream (external-process-input-stream process)
                 :outstream (external-process-output-stream process)
                 :prompt prompt)
      ; Set the prompt so we can use it as a delimiter
      (pycmd pyserver "import sys ; sys.ps1='~a\\n' ; print" prompt)
      pyserver))

(define-method (pycmd (p python-server instream outstream prompt) s &rest args)
  (apply 'format instream s args)
  (terpri instream)
  (terpri instream)
  (force-output instream)
  (with-collector collect
    (do ((line (read-line outstream) (read-line outstream)))
        ((and (>= (length line) (length prompt))
              (string= prompt (slice line (- (length prompt))))))
      (collect line))
    (sleep 0.01)
    (while (listen outstream) (read-char outstream))))

(define-method (kill (p python-server instream))
  (close instream))

(define-method (status (p python-server process))
  (external-process-status process))

(defv $python-server nil)

(defun insure-python-server (&key (path "python") (prompt $default-python-prompt))
  (or (and $python-server (eq (status $python-server) :running))
      (setf $python-server (make-python-server :path path :prompt prompt))))

(defun py (cmd &rest args)
  (insure-python-server)
  (apply 'pycmd $python-server cmd args))
