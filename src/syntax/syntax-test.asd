(in-package :asdf)

(defsystem burning-syntax-test
  :depends-on (:burning-syntax :burning-lexical :burning-testing)
  :components ((:file "test-package")
	       (:file "core-test" :depends-on ("test-package"))
	       (:file "language-test" :depends-on ("core-test" "test-package"))))