(defpackage #:burning-btr
  (:use #:common-lisp #:burning-command-line #:burning-filesystem #:burning-xml)
  (:export #:*base-filesystem*
	   #:repository
	   #:make-repository
	   #:repository-version

	   #:entity
	   #:entity-name
	   #:entities

	   #:wrong-xml-node-name
	   #:wrong-xml-node-name-expected
	   #:wrong-xml-node-name-got

	   #:missed-xml-attribute-warning
	   #:missed-xml-attribute-warning-name
	   #:missed-xml-attribute-warning-node-name

	   #:add-entity
	   #:remove-entity

	   #:unit
	   #:make-unit
	   #:unit-name
	   #:unit-files

	   #:group
	   #:make-group
	   #:group-name

	   #:entity-with-same-name-already-exists
	   #:entity-with-same-name-already-exists-name

	   #:*repository-class*
	   #:*unit-class*

	   #:define-btr-class

	   #:repository-path
	   #:write-repository

	   #:btr-run

	   #:no-action-specified-error

	   #:too-much-actions-specified-error
	   #:too-much-actions-specified-error-actions

	   #:repository-already-exists-error
	   #:repository-already-exists-error-path

	   #:repository-does-not-exist-error
	   #:repository-does-not-exist-error-path

	   #:path-is-not-in-repository-error
	   #:path-is-not-in-repository-error-path
	   #:path-is-not-in-repository-error-repository-path))



