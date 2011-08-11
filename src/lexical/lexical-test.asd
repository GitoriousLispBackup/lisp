(in-package :asdf)

(defsystem burning-lexical-test
    :description "Tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :depends-on (:burning-testing :burning-lexical)
    :components ((:file "package-test")
		 (:file "input-test" :depends-on ("package-test"))
		 (:file "reg-expr-test" :depends-on ("package-test"))
		 (:file "lexical-test" :depends-on ("package-test"))))