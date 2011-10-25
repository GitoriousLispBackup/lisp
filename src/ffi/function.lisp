(in-package :burning-ffi)

(defvar *ffi-types* ())

(defun %add-type (external-value internal-value)
  (if (not (equal (%get-type external-value) internal-value))
      (pushnew `(,external-value . ,internal-value) *ffi-types*)))

(defun %get-type (external-value)
  (rest (assoc external-value *ffi-types* :test #'equal)))

(defmacro deffitype (external-name internal-name)
  `(%add-type ,external-name ,internal-name))

(defgeneric get-type (name &rest args))

(defmethod get-type (name &rest args)
  (declare (ignore args))
  (%get-type name))

(deffitype :void :void)
(deffitype :bool :boolean)
(deffitype :int :int)
(deffitype :short :short)
(deffitype :long :long)
(deffitype :uint :uint)
(deffitype :ushort :ushort)
(deffitype :ulong :ulong)
(deffitype :float :float)
(deffitype :double :double)

(defmacro foreign-lambda-wrapper (return-value &rest args)
  `(,return-value ,args))

(defmacro lambda-callback-wrapper (return-value &rest args)
  `(,return-value ,args))

(defun make-functional-type (return-value &rest args)
  (eval `(cffi:defctype ,(gensym "FUNCTIONAL") 
	     (:wrapper :pointer 
		       :from-c (foreign-lambda-wrapper ,return-value ,args)
		       :to-c (lambda-callback-wrapper ,return-value ,args)))))

(defmethod get-type ((name (eql :function)) &rest args)
  (if (%get-type args)
      (%get-type args)
      (progn 
	(%add-type args (apply #'make-functional-type args))
	(%get-type args))))

(defun ffi-type (argument)
  (let ((type-name (if (atom argument) argument (first argument)))
	(arguments (if (atom argument) nil (rest argument))))
    (let ((value (apply #'get-type type-name arguments)))
      (if value 
	  value
	  (error "Wrong ffi type - ~a." argument)))))

(defaction function (name-and-type arguments)
  (if (/= (length name-and-type) 2) (error "Wrong parameters count in call to :function"))
  (flet ((parse-argument (argument)
	   (let ((last (ffi-type (first (last argument)))))
	     (mapcar #'(lambda (x) (list x last)) (butlast argument)))))
    (eval `(cffi:defcfun ,(first name-and-type) ,(ffi-type (second name-and-type))
	     ,@(reduce #'append (mapcar #'parse-argument arguments))))))
