(in-package :asdf)

(defsystem burning-lexical
    :description "A burning library working with lexic"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :components ((:file "package")
                 (:file "reg-expr" :depends-on ("package"))
		 (:file "lexical" :depends-on ("package" "reg-expr"))))