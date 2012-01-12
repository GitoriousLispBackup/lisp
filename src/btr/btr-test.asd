(in-package #:asdf)

(defsystem #:burning-btr-test
  :description "A tests"
  :version "0.1"
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :serial t
  :components ((:file "test-package")
	       (:file "repository-test")
	       (:file "extensions-test"))
  :depends-on (#:burning-btr #:burning-testing))
