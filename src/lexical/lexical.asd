(in-package :asdf)

(defsystem burning-lexical
    :description "A burning library working with lexic"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :components ((:file "package")
		 (:file "input" :depends-on ("package"))
		 (:file "core" :depends-on ("package"))
		 (:file "regular-language" :depends-on ("package" "core"))
                 (:file "reg-expr" :depends-on ("package" "core"))
		 (:file "lexical" :depends-on ("package" "reg-expr" "input" "core"))))
