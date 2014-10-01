
(require :clos-utils)

(defun random-prompt (&optional (n 100))
  (strcat "--" (b32 (random (ash 1 n))) "--"))

(define-class bash-server process instream outstream prompt (lock (make-lock)))

(define-print-method (bash-server process) "#<Bash server ~A ~S>"
  (ccl::external-process-pid process)
  (external-process-status process))

(define-method (flush (server bash-server outstream))
  (if (listen outstream)
    (with-char-collector c (while (listen outstream) (c (read-char outstream))))))

(defun make-bash-server (&key (path "bash") (prompt (random-prompt)))
  (bb process (run-program path '()
                           :sharing :lock ; For now
                           :input :stream :output :stream :wait nil
                           :external-format :utf-8
                           :env '())
      (make-instance 'bash-server :process process
        :instream (external-process-input-stream process)
        :outstream (external-process-output-stream process)
        :prompt prompt)))

(define-method (cmd (server bash-server instream outstream prompt lock) s &rest args)
  (with-lock-grabbed (lock)
    (aif (flush server) (logmsg "Extra characters on ~A: ~A" server it))
    (format instream "~A ; echo ~A~%" s prompt)
    (force-output instream)
    (bb l (split s #\Space)
        (when (and (= (length l) 2) (string= (1st l) "ssh"))
          (sleep 0.1)
          (format instream "echo ~A~%" prompt)
          (force-output instream)))
    (bb verbose (1st args)
        (when verbose (format t "~&BASH: ~A~&" s) (force-output))
        result (with-collector collect
                 (flet ((rl ()
                          #+windows (string-trim #.(string #\return) (read-line outstream))
                          #-windows (read-line outstream)))
                   (do ((line (rl) (rl)))
                       ((string= line prompt))
                     (when verbose
                       (if (eq verbose t)
                         (progn (princ line) (terpri) (force-output))
                         (progn (princ verbose) (force-output))))
                     (unless (eq verbose t) (collect line)))))
        (sleep 0.001)
        (if (eq verbose t) (values) (values result (flush server))))))

(define-method (close (p bash-server instream) &key abort)
  (close instream))

(define-method (status (p bash-server process))
  (external-process-status process))

#+nil(progn

(setf b (make-bash-server))
(cmd b "ps -o %mem ~A" (ccl::getpid))

)
