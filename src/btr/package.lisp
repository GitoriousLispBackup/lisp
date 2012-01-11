(defpackage #:burning-btr
  (:use #:common-lisp #:burning-command-line #:burning-filesystem #:burning-xml)
  (:export #:*base-filesystem*
	   #:repository
	   #:make-repository
	   #:repository-version
	   #:entities

	   #:wrong-repository-node-name
	   #:wrong-repository-node-name-name

	   #:repository-api-version-does-not-specified

	   #:add-entity
	   #:remove-entity

	   #:make-unit
	   #:unit-name
	   #:unit-files

f	   #:unit-with-same-name-already-exists
	   #:unit-with-same-name-already-exists-name))

