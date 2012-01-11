(defpackage #:burning-btr
  (:use #:common-lisp #:burning-command-line #:burning-filesystem #:burning-xml)
  (:export #:*base-filesystem*
	   #:repository
	   #:make-repository
	   #:repository-version
	   #:entities))