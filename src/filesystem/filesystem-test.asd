(in-package :asdf)

(defsystem burning-filesystem-test
    :description "Cross-implementation library to work with filesystems."
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "test-package")
		 (:module "vfs"
			  :components
			  ((:file "base-test")
			   (:file "file-stream-test")))
		 (:file "user-interface-test"))
    :depends-on (:burning-testing :burning-filesystem))


