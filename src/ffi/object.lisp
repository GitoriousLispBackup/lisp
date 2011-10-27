(in-package :burning-ffi)

(defun c-name-to-lisp (name)
  (string-upcase (substitute #\- #\_ name)))

(defclass ffi-object ()
  ((raw-value :initarg :raw-value :reader ffi-object-value)))

(defmethod initialize-instance :after ((object ffi-object) &rest initargs &key other-keys)
  (declare (ignore initargs other-keys))
  (let ((value (ffi-object-value object)))
    (tg:finalize object #'(lambda () (cffi:foreign-free value)))))

(defgeneric delete-object (object))

(defmethod delete-object ((object ffi-object))
  t)

(defmethod delete-object :after ((object ffi-object))
  (tg:cancel-finalization object)
  (cffi:foreign-free (ffi-object-value object)))

(defun ensure-type (object type)
  (unless (or (typep object type) (not object))
    (error "Cannot convert object ~a to type ~a." object type)))

(cffi:defcstruct c-ffi-object
  (value :pointer)
  (type :pointer))

(defun make-ffi-object (class object)
  (let ((value (cffi:foreign-slot-value object 'c-ffi-object 'value))
	(type (cffi:foreign-slot-value object 'c-ffi-object 'type)))
    (cffi:foreign-free object)
    (make-instance class :raw-value value)))

(defun def-class-ctype (class-name)
  (eval `(cffi:defctype ,(gensym (symbol-name class-name)) 
	     (:wrapper :pointer
		       :from-c ,#'(lambda (x) 
				    (if (cffi:null-pointer-p x)
					nil
					(make-ffi-object class-name x)))
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
	    (let ((destructor-name (apply #'parse-destructor (rest (first destructors)))))
	      (eval `(defmethod delete-object ((object ,name))
		       (funcall #',destructor-name object)))))))))

(defmacro with-object ((name init-form) &body body)
  `(let (,name) 
     (unwind-protect 
	  (progn
	    (setf ,name ,init-form)
	    ,@body)
       (delete-object ,name))))

;;
;; Class UUID's
;;

(defvar *uuid-table* (make-hash-table))

(defgeneric do-generate-uuid (action &rest args))

(defmethod do-generate-uuid (action &rest args)
  (declare (ignore action args))
  nil)

(defmethod do-ffi-generate-uuid ((action (eql ':class)) &rest args)
  (let ((class-name (first (first args))))
    `((:uuid ,class-name ,(generate-uuid)))))

(defun ffi-generate-uuid (&rest actions)
  (reduce 'append (mapcar #'(lambda (x) (do-ffi-generate-uuid (first x) (rest x))) actions)))

(defun generate-uuid-file (filename &rest actions)
  (let ((uuids (apply #'ffi-generate-uuid actions)))
    (with-open-file (file filename :direction :output :if-exists :supersede)
      (mapc #'(lambda (x) (format file "~a~%" x)) uuids))
    (with-open-file (file (concatenate 'string filename ".h") :direction :output :if-exists :supersede)
      (labels ((print-uuid (uuid)
		 (cond 
		   ((null uuid) t)
		   ((null (rest uuid)) (format nil "~a" (first uuid)))
		   (t (concatenate 'string (format nil "~a, " (first uuid)) (print-uuid (rest uuid))))))
	       (print-uuid-string (uuid)
		 (format file "const unsigned char ~a_uuid [] = { ~a };~%" 
			 (second uuid) 
			 (print-uuid (%uuid-int-to-list (third uuid))))))
	(mapc #'print-uuid-string uuids)))))

(defun load-ffi-uuid (&rest actions)
  (mapc #'(lambda (x) (setf (gethash (third x) *uuid-table*)
			    (intern (c-name-to-lisp (second x))))) actions)
  (print *uuid-table*))





       
       