(defpackage #:burning-command-line
  (:use #:common-lisp)
  (:export #:make-argument
	   #:argument-description
	   #:argument-short-name

	   #:make-argument-list
	   #:add-argument
	   #:add-arguments
	   #:argument
	   #:have-argument-p
	   #:argument-set-p
	   #:help-message

	   #:argument-not-defined-error
	   #:argument-not-defined-error-name
	   
	   #:argument-already-exists-error
	   #:argument-already-exists-error-name

	   #:short-name-already-exists-error
	   #:short-name-already-exists-error-char))
	   