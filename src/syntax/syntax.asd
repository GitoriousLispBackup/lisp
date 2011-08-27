(in-package :asdf)

(defsystem burning-syntax
  :description "A burning generator of syntax analyzers"
  :version "0.1"
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :depends-on (:burning-lexical)
  :components ((:file "package")
	       (:file "language" :depends-on ("package"))))