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
