(in-package :burning-ffi-test)

(defcase object-test)

(load-ffi "~/lisp/src/ffi/simple-ffi.ffi")
(load-uuid "~/lisp/src/ffi/c/handler.uuid")

(deftest object-test simple-object ()
  (let ((object (make-holder)))
    (hld-set-holded object 7)
    (!= (hld-holded object) 7)
    (hld-set-holded object 12)
    (!= (hld-holded object) 12)))

(deftest object-test two-destructors ()
  (!error (burning-ffi::load-ffi-actions '(:class "wrong_class" (:destructor "dst1") (:destructor "dst2")))
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
