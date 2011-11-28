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

	   parent-path
	   root-path
	   make-path
	   path+
	   path-as-file
	   path-as-directory

	   path-exists-p
	   correct-path-p

	   make-file
	   make-directory

	   file-lock-error
	   file-lock-error-path
	   wrong-filename-error
	   wrong-filename-error-string
	   directory-does-not-exist-error
	   directory-does-not-exist-error-path
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
