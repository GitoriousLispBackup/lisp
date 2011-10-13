(in-package :asdf)

(defsystem burning-threads
    :description "Cross-implementation library for threading routines"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :components ((:file "package")
		 (:file "base" :depends-on ("package"))))

