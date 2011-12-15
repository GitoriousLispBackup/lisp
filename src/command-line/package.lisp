(defpackage #:burning-command-line
  (:use #:common-lisp)
  (:export #:make-argument
	   #:argument-description

	   #:make-argument-list
	   #:add-argument
	   #:add-arguments
	   #:argument
	   #:have-argument-p
	   #:argument-set-p

	   #:argument-not-defined-error
	   #:argument-not-defined-error-name
	   
	   #:argument-already-exists-error
	   #:argument-already-exists-error-name))
	   