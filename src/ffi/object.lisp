(in-package :burning-ffi)

(defun c-name-to-lisp (name)
  (string-upcase (substitute #\- #\_ name)))

(defclass ffi-object ()
  ((raw-value :initarg :raw-value :reader ffi-object-value)))

(defun ensure-type (object type)
  (unless (or (typep object type) (not object))
    (error "Cannot convert object ~a to type ~a." object type)))

(defun def-class-ctype (class-name)
  (eval `(cffi:defctype ,(gensym (symbol-name class-name)) 
	     (:wrapper :pointer
		       :from-c ,#'(lambda (x) 
				    (if (cffi:null-pointer-p x)
					nil
					(make-instance class-name :raw-value x)))
		       :to-c ,#'(lambda (x)
				  (ensure-type x class-name)
				  (if (null x) (cffi:null-pointer) (ffi-object-value x)))))))

(defaction class (name &rest options)
  (let ((name (intern (c-name-to-lisp name))))
    (flet ((parse-constructor (constructor-name arguments)
	     (do-ffi-action :function `(,constructor-name ,name) arguments))
	   (parse-destructor (destructor-name)
	     (do-ffi-action :function `(,destructor-name :void) `((object ,name)))))
      (eval `(defclass ,name (ffi-object) ()))
      (%add-type name (def-class-ctype name))
      (mapc #'(lambda (x) (apply #'parse-constructor (rest x))) 
	    (remove-if-not #'(lambda (x) (eq (first x) :constructor)) options))
      (let ((destructors (remove-if-not #'(lambda (x) (eq (first x) :destructor)) options)))
	(when (> (length destructors) 1)
	  (error "Cannot define more than one destructor for class ~a." name))
	(if destructors
	    (apply #'parse-destructor (rest (first destructors))))))))

       
       