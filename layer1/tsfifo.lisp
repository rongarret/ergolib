;;;;;;;;;;;;;;;;;
;;;
;;; Thread-safe FIFO
;;;

(require :ergolib)

(defstruct tsfifo first last
  (semaphore (make-semaphore))
  (cnt 0)
  (opcnt 0)
  (lock (make-lock)))

(define-print-method (tsfifo cnt) "#<TSFIFO with ~A elements>" cnt)

(defstruct fifo-node data next)

(define-print-method (fifo-node data) "#<FIFO node ~A>" data)

(define-method (size (tsf tsfifo cnt)) cnt)

(def-bb-clause :with-lock (lock &body body)
  `(with-lock-grabbed (,lock) (%bb ,@body)))

(defun fpush (thing fifo)
  (bb node (make-fifo-node :data thing)
      :with-slots (lock semaphore cnt opcnt) fifo
      cnt0 cnt
      (if (> cnt0 1000) (sleep (* (- cnt0 1000) 1e-6)))
      :with-lock lock
      :with-slots (first last) fifo
      (if last
        (setf (slot-value last 'next) node)
        (setf first node))
      (setf last node)
      (incf cnt)
      (incf opcnt)
      (signal-semaphore semaphore)
      fifo))

(defun sem-wait (semaphore &optional timeout)
  (if timeout
    (timed-wait-on-semaphore semaphore timeout)
    (progn (wait-on-semaphore semaphore) t)))

(defun fpop (fifo &optional timeout)
  (declare (optimize (speed 3) (safety 0)))
  (bb :with-slots (semaphore) fifo
      (unless (sem-wait semaphore timeout) (return (values nil nil)))
      :with-slots (lock cnt opcnt) fifo
      :with-lock lock
      :with-slots (first last) fifo
      :with-slots (next) first
      result first
      (if (eq first last)
        (setf first nil last nil)
        (setf first next))
      (decf cnt)
      (incf opcnt)
      (values (fifo-node-data result) t)))

#|
(defv tsf (make-tsfifo))

(progn
  (process-run-function "push" (fn () (dotimes (i 10000000) (fpush i tsf))))
  (process-run-function "pop" (fn () (dotimes (i 10000000) (fpop tsf) (sleep 0.1)))))
|#
