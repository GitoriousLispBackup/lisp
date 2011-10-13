(defpackage :burning-threads
  (:use :common-lisp)
  (:export make-thread
	   spawn-thread
	   wait-thread
	   wait-threads))
