(in-package :asdf)

(defsystem burning-syntax-test
  :depends-on (:burning-syntax :burning-lexical :burning-testing)
  :components ((:file "test-package")
	       (:file "language-test")))