(in-package :burning-threads-test)

(defmacro times (count &body body)
  (let ((result ()))
    (dotimes (i count)
      (push `(progn ,@body) result))
    `(list ,@result)))

(defcase mutex-test)

(deftest mutex-test simple-lock ()
  (let ((result-queue ())
	(threads nil)
	(lock (make-mutex "queue lock")))
    (dotimes (i 10)
      (push (spawn-thread #'(lambda () 
			      (grab-mutex lock)
			      (dotimes (i 10)
				(thread-random)
				(push i result-queue))
			      (release-mutex lock)))
	    threads))
    (apply #'wait-threads threads)
    (!equal result-queue (apply #'append (times 10 (range 0 9 :accending nil))))))

(deftest mutex-test with-mutex-test ()
  (let ((result 0)
	(mutex (make-mutex)))
    (block mutex-block
      (with-mutex mutex
	(wait-thread (spawn-thread #'(lambda ()
				       (with-timeout (0.5 (incf result)) 
					 (grab-mutex mutex)
					 (decf result)))))
	(return-from mutex-block)))
    (!= result 1)
    (wait-thread (spawn-thread #'(lambda ()
				   (with-timeout (0.5 (decf result))
				     (grab-mutex mutex)
				     (incf result)
				     (release-mutex mutex)))))
    (!= result 2)))
    
(deftest mutex-test simple-shared-variable ()
  (let ((result (make-shared-variable 0)))
    (flet ((client ()
	     (dotimes (i 10)
	       (thread-random)
	       (with-shared-variable result
		 (incf result)))))
      (let ((threads ()))
	(dotimes (i 500)
	  (push (spawn-thread #'client) threads))
	(apply #'wait-threads threads))
      (with-shared-variable result
	(!= result 5000)))))

(deftest mutex-test complex-shared-variable ()
  (let ((variables (list (make-shared-variable "value"))))
    (with-shared-variable (value (first variables))
      (!equal value "value"))))