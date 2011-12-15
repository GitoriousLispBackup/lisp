(defpackage :burning-filesystem
  (:use :common-lisp :trivial-gray-streams)
  (:export make-directory-path
	   directory-path
	   directory-host
	   directory-device
	   directory-path

	   make-file-path
	   file-path
	   file-directory
	   file-name
	   file-type
	   file-version

	   path-from-string
	   path-to-string

	   file-path-p
	   directory-path-p

	   path-host
	   path-device
	   path-path
	   path-directory
	   path-name
	   path-type
	   path-version

	   parent-path
	   root-path
	   copy-path
	   path+
	   path-as-file
	   path-as-directory
	   relative-path-p
	   absolute-path-p

	   path-exists-p
	   correct-path-p

	   make-file
	   make-directory
	   remove-file
	   remove-directory

	   list-directory
	   resolve-path
	   as-absolute-path

	   home-directory
	   current-directory

	   open-file
	   close-stream
	   with-file

	   file-lock-error
	   file-lock-error-path
	   wrong-filename-error
	   wrong-filename-error-string
	   directory-does-not-exist-error
	   directory-does-not-exist-error-path
	   directory-not-empty-error
	   directory-not-empty-error-path
	   file-does-not-exist-error
	   file-does-not-exist-error-path
	   wrong-file-path-error
	   wrong-file-path-error-path

	   fs-path-from-string
	   fs-path-to-string
	   fs-as-file-path
	   fs-as-directory-path
	   fs-file-exists-p
	   fs-directory-exists-p
	   fs-list-directory
	   fs-current-directory
	   fs-home-directory
	   fs-delete-file
	   fs-delete-directory
	   fs-make-file
	   fs-make-directory
	   fs-open-file
	   fs-open-input-stream
	   fs-open-output-stream
	   fs-open-io-stream
	   fs-close-stream
	   fs-file-length

	   common-filesystem

	   make-virtual-filesystem
	   vfs-cat))
