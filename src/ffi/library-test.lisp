(in-package :burning-ffi-test)

(defcase library-test)

(deftest library-test load-test ()
  (let ((action '(:library "glib-2.0")))
    (!condition-safe (load-ffi action)))
  (let ((action '(:library "simple" :path "~/lisp/src/ffi/c/")))
    (!condition-safe (load-ffi action))))