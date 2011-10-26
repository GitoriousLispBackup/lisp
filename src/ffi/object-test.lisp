(in-package :burning-ffi-test)

(defcase object-test)

(load-ffi '(:library "simple" :path "~/lisp/src/ffi/c/")
	  '(:class "int_holder" (:constructor "make_holder" ()) (:destructor "delete_holder"))
	  '(:function ("hld_holded" :int) ((holder int-holder)))
	  '(:function ("hld_set_holded" :void) ((holder int-holder) (value :int))))

(deftest object-test simple-object ()
  (let ((object (make-holder)))
    (hld-set-holded object 7)
    (!= (hld-holded object) 7)
    (hld-set-holded object 12)
    (!= (hld-holded object) 12)))

(deftest object-test two-destructors ()
  (!error (load-ffi '(:class "wrong_class" (:destructor "dst1") (:destructor "dst2")))
	  "Cannot define more than one destructor for class WRONG-CLASS."))
