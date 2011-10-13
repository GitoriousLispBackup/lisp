;;;
;;; Base threading routines
;;;

(in-package :burning-threads)

(defun no-threads-error ()
  (error "No threads implementation for ~a." (lisp-implementation-type)))

;; Makes new thread 

(defun make-thread (function &key (name "unnamed thread") (arguments ()))
  #+clisp
  (mt:make-thread #'(lambda () (apply function arguments)) :name name)
  #+sbcl
  (sb-thread:make-thread function :name name :arguments arguments)
  #+ccl
  (let ((process (ccl:make-process name)))
    (apply #'ccl:process-preset process function arguments)
    (ccl:process-enable process)
    process)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun spawn-thread (function &rest args)
  (make-thread function :arguments args))

;;Waits for 'thread'

(defun wait-thread (thread)
  #+clisp
  (mt:thread-join thread)
  #+sbcl
  (sb-thread:join-thread thread)
  #+ccl
  (ccl:join-process thread)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun wait-threads (&rest threads)
  (dolist (thread threads)
    (wait-thread thread)))

(defun thread-sleep (thread)
  #+clisp
  ()
  #-clisp
  ())