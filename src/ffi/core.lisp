(in-package :burning-ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-keyword (symbol)
    (intern (symbol-name symbol) "KEYWORD")))

(defgeneric do-ffi-action (action &rest arguments))

(defmacro defaction (name (&rest arguments) &body body)
  (let ((action-sym (gensym))
	(arguments-sym (gensym)))
    `(defmethod do-ffi-action ((,action-sym (eql ',(intern-keyword name))) &rest ,arguments-sym)
       (declare (ignore ,action-sym))
       (labels ((this ,arguments
		,@body))
	 (apply #'this ,arguments-sym)))))

(defaction library (name &key (path nil))
  (if path
      (push path cffi:*foreign-library-directories*))
  (let ((library (intern name)))
    (eval
     `(progn
	(cffi:define-foreign-library ,library
	  (t (:default ,(concatenate 'string "lib" name))))
	(cffi:use-foreign-library ,library)))))


(defun load-ffi (&rest actions)
  (mapc #'(lambda (x) (apply #'do-ffi-action x)) actions))

