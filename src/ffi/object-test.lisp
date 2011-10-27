(in-package :burning-ffi-test)

(defcase object-test)

(defconstant object-ffi
  '((:library "simple" :path "~/lisp/src/ffi/c/")
    (:class "int_holder" (:constructor "make_holder" ()) (:destructor "delete_holder"))
    (:function ("hld_holded" :int) ((holder int-holder)))
    (:function ("hld_set_holded" :void) ((holder int-holder) (value :int)))
    (:class "mega_holder" (:inherit "int_holder"))
    (:function ("make_mega_holder" int-holder) ())))

(apply #'load-ffi object-ffi)

(deftest object-test simple-object ()
  (let ((object (make-holder)))
    (hld-set-holded object 7)
    (!= (hld-holded object) 7)
    (hld-set-holded object 12)
    (!= (hld-holded object) 12)))

(deftest object-test two-destructors ()
  (!error (load-ffi '(:class "wrong_class" (:destructor "dst1") (:destructor "dst2")))
	  "Cannot define more than one destructor for class WRONG-CLASS."))

(deftest object-test with-object-test ()
  (!= (with-object (holder (make-holder))
	(hld-set-holded holder 12)
	(hld-holded holder))
      12))

(deftest object-test inheritance-test ()
  (let ((base-object (make-holder))
	(derived-object (make-mega-holder)))
    (!t (typep base-object 'int-holder))
    (!null (typep base-object 'mega-holder))
    (!t (typep derived-object 'int-holder))
    (!t (typep derived-object 'mega-holder))))

