(in-package :asdf)

(defsystem burning-ffi-test
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :components ((:file "test-package")
		 (:file "library-test" :depends-on ("test-package"))
		 (:file "function-test" :depends-on ("test-package")))
    :depends-on (:burning-testing :burning-ffi))