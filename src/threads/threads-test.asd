(in-package :asdf)

(defsystem burning-threads-test
    :description "Tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :components ((:file "test-package")
		 (:file "base-test" :depends-on ("test-package")))
    :depends-on (:burning-testing :burning-threads))

