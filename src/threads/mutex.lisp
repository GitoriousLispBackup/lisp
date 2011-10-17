(in-package :burning-threads)

(defun make-mutex (&optional (name "mutex"))
  #+clisp
  (mt:make-mutex :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+ccl
  (ccl:make-lock name)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun grab-mutex (mutex)
  #+clisp
  (mt:mutex-lock mutex)
  #+sbcl
  (sb-thread:grab-mutex mutex)
  #+ccl
  (ccl:grab-lock mutex)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun release-mutex (mutex)
  #+clisp
  (mt:mutex-unlock mutex)
  #+sbcl
  (sb-thread:release-mutex mutex)
  #+ccl
  (ccl:release-lock mutex)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defmacro with-mutex (mutex &body body)
  #+clisp
  `(mt:with-mutex-lock (,mutex)
     ,@body)
  #+sbcl
  `(sb-thread:with-mutex (,mutex)
     ,@body)
  #+ccl
  `(ccl:with-lock-grabbed (,mutex)
     ,@body)
  #-(or clisp sbcl ccl)
  `(no-threads-error))
