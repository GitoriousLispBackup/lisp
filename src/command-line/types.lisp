(in-package #:burning-command-line)

(defgeneric parse-type (values type &rest type-args))
(defgeneric parse-type-value (value type &rest type-args))

(defgeneric type-value-name (type &rest type-args))

(defmethod type-value-name (type &rest type-args)
  (declare (ignore type type-args))
  (symbol-name type))

(defmethod type-value-name ((type cons) &rest type-args)
  (declare (ignore type-args))
  (apply #'type-value-name (first type) (rest type)))

;;
;; General parsing methods
;;

(define-condition missed-key-value-error (cmd-parsing-error)
  ((type :initarg :type :reader missed-key-value-error-type)))

(define-condition wrong-key-value-error (cmd-parsing-error)
  ((value :initarg :value :reader wrong-key-value-error-value)
   (type :initarg :type :reader wrong-key-value-error-type)))

(defmethod parse-type (value type &rest type-args)
  (let ((arg (iterator-current value)))
    (assert (listp arg))
    (unless (eq (first arg) :param)
      (error 'missed-key-value-error :type (if type-args (cons type type-args) type)))
    (assert (eq (length arg) 2))
    (handler-case (let ((result (apply #'parse-type-value (second arg) type type-args)))
		    (iterator-next value)
		    result)
      (wrong-key-value-error (err)
	(unless (slot-boundp err 'value)
	  (setf (slot-value err 'value) (second arg)))
	(unless (slot-boundp err 'type)
	  (setf (slot-value err 'type) (if type-args (cons type type-args) type)))
	(error err))
      (error (err)
	(error 'wrong-key-value-error :type (if type-args (cons type type-args) type) :value (second arg))))))

;;
;; Parsers for common types
;;

(defmethod parse-type-value (value (type (eql 'string)) &rest type-args)
  (assert (null type-args))
  value)

(define-condition argument-value-too-low-error (wrong-key-value-error)
  ((min-value :initarg :min-value :reader argument-value-too-low-error-min-value)))

(define-condition argument-value-too-high-error (wrong-key-value-error)
  ((max-value :initarg :max-value :reader argument-value-too-high-error-max-value)))

(defmethod parse-type-value (value (type (eql 'integer)) &rest type-args)
  (flet ((check-lower-bound (int bound)
	   (when (< int bound)
	     (error 'argument-value-too-low-error :min-value bound :value int)))
	 (check-higher-bound (int bound)
	   (when (> int bound)
	     (error 'argument-value-too-high-error :max-value bound :value int))))
    (assert (<= (length type-args) 2))
    (let ((value (handler-case (parse-integer value) (error () (error 'wrong-key-value-error)))))
      (when (>= (length type-args) 1) (check-lower-bound value (first type-args)))
      (when (>= (length type-args) 2) (check-higher-bound value (second type-args)))
      value)))

(defmethod parse-type-value (value (type (eql 'float)) &rest type-args)
  (assert (null type-args))
  (parse-real-number value))

;;
;; Parser for list type
;;

(defun parameter-p (arg)
  (and (listp arg) (= (length arg) 2) (eq (first arg) :param)))

(defmethod type-value-name ((type (eql 'list)) &rest type-args)
  (format nil "[~a ...]" (type-value-name (first type-args))))

(defmethod parse-type (value (type (eql 'list)) &rest type-args)
  (assert (= (length type-args) 1))
  (if (parameter-p (iterator-current value))
      (cons (apply #'parse-type value type-args) (apply #'parse-type value type type-args))
      nil))

;;
;; Parser for tuples  
;;

(defmethod type-value-name ((type (eql 'tuple)) &rest type-args)
  (flet ((argument-name (arg)
	   (format nil " ~a" arg)))
    (subseq (apply #'string+ (mapcar #'argument-name type-args)) 1)))

(defmethod parse-type (value (type (eql 'tuple)) &rest type-args)
  (flet ((as-list (value)
	   (if (atom value)
	       (list value)
	       value)))
    (if type-args
	(cons (apply #'parse-type value (as-list (first type-args)))
	      (apply #'parse-type value 'tuple (rest type-args))))))

