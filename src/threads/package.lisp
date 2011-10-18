(defpackage :burning-threads
  (:use :common-lisp)
  (:export current-thread
	   make-thread
	   spawn-thread
	   wait-thread
	   wait-threads
	   thread-yield
	   with-timeout
	   thread-random
	   
	   make-mutex
	   grab-mutex
	   release-mutex
	   with-mutex
	   make-shared-variable
	   with-shared-variable

	   make-semaphore
	   wait-semaphore
	   signal-semaphore

	   make-condition-variable
	   with-condition-variable
	   wait-condition
	   signal-condition
	   broadcast-condition
	   
	   defmonitor
	   defmfun))

