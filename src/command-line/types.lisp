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

(define-condition wrong-key-value-error (error)
  ((value :initarg :value :reader wrong-key-value-error-value)
   (type :initarg :type :reader wrong-key-value-error-type)))

(defmethod parse-type (value type &rest type-args)
  (let ((arg (first value)))
    (assert (listp arg))
    (unless (eq (first arg) :param)
      (error 'missed-key-value-error :type (if type-args (cons type type-args) type)))
    (assert (eq (length arg) 2))
    (handler-case (values (apply #'parse-type-value (second arg) type type-args) (rest value))
      (error ()
	(error 'wrong-key-value-error
	       :value (second arg)
	       :type (if type-args (cons type type-args) type))))))

;;
;; Parsers for common types
;;

(defmethod parse-type-value (value (type (eql 'string)) &rest type-args)
  (assert (null type-args))
  value)

(defmethod parse-type-value (value (type (eql 'integer)) &rest type-args)
  (assert (null type-args))
  (parse-integer value))

;;
;; Parser for list type
;;

(defun parameter-p (arg)
  (and (listp arg) (= (length arg) 2) (eq (first arg) :param)))

(defmethod type-value-name ((type (eql 'list)) &rest type-args)
  (format nil "[~a ...]" (type-value-name (first type-args))))

(defmethod parse-type (value (type (eql 'list)) &rest type-args)
  (assert (= (length type-args) 1))
  (if (parameter-p (first value))
      (multiple-value-bind (result rest) (apply #'parse-type value type-args)
	(multiple-value-bind (rest-result remain) (apply #'parse-type (rest value) type type-args)
	  (values (cons result rest-result) remain)))
      (values nil value)))

  