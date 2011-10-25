(in-package :asdf)

(defsystem burning-ffi
    :description "Library for operating with C-side functions."
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :components ((:file "package")
		 (:file "core" :depends-on ("package"))
		 (:file "function" :depends-on ("package" "core")))
    :depends-on (:cffi))