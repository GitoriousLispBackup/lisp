(defpackage #:burning-command-line
  (:use #:common-lisp)
  (:export #:make-argument
	   #:argument-name
	   #:argument-description
	   #:argument-short-name

	   #:make-arguments-spec
	   #:add-argument
	   #:add-arguments
	   #:argument
	   #:have-argument-p
	   #:help-message
	   #:parse-arguments
	   
	   #:argument-set-p
	   #:argument-value

	   #:argument-already-exists-error
	   #:argument-already-exists-error-name

	   #:short-name-already-exists-error
	   #:short-name-already-exists-error-char

	   #:wrong-argument-error
	   #:wrong-argument-error-string

	   #:wrong-short-argument-error
	   #:wrong-short-argument-error-char

	   #:wrong-key-value-error
	   #:wrong-key-value-error-value
	   #:wrong-key-value-error-type

	   #:missed-key-value-error
	   #:missed-key-value-error-type

	   #:too-few-arguments-in-group-set
	   #:too-few-arguments-in-group-set-group

	   #:too-much-arguments-in-group-set
	   #:too-much-arguments-in-group-set-group
	   #:too-much-arguments-in-group-set-arguments))
	   