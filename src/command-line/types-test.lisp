(in-package #:burning-command-line-test)

(defcase types-test)

(defmacro !parse ((&rest args) spec &body values)
  (let ((args-sym (gensym)))
    (flet ((value-check (value)
	     (destructuring-bind (test name value) value
	       `(,test (argument-value ,name ,args-sym) ,value))))
      `(let ((,args-sym (parse-arguments ',args ,spec)))
	 ,@(mapcar #'value-check values)))))

(deftest types-test integer-type-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'integer))))
    (!parse ("--key" "12345") spec (!= "key" 12345))
    (!condition (parse-arguments '("--key" "bla") spec)
		wrong-key-value-error
		(cmd-parsing-error-argument "key")
		(wrong-key-value-error-value "bla")
		(wrong-key-value-error-type 'integer))))

(deftest types-test bounded-integers-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type '(integer -100 1000)))))
    (!parse ("--key" "123") spec (!= "key" 123))
    (!condition (parse-arguments '("--key" "-101") spec)
		argument-value-too-low-error
		(cmd-parsing-error-argument "key")
		(wrong-key-value-error-value -101)
		(wrong-key-value-error-type '(integer -100 1000))
		(argument-value-too-low-error-min-value -100))
    (!condition (parse-arguments '("--key" "1001") spec)
		argument-value-too-high-error
		(cmd-parsing-error-argument "key")
		(wrong-key-value-error-value 1001)
		(argument-value-too-high-error-max-value 1000))))

(deftest types-test string-keys-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'string))))
    (!parse ("--key" "blabla") spec (!equal "key" "blabla"))))

(deftest types-test float-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'float))))
    (!parse ("--key" "100.001") spec (!= "key" 100.001))
    (!condition (parse-arguments '("--key" "bla") spec)
		wrong-key-value-error
		(cmd-parsing-error-argument "key")
		(wrong-key-value-error-value "bla")
		(wrong-key-value-error-type 'float))))

(deftest types-test negavie-floats-parsing ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'float))))
    (!parse ("--key" "-0.1") spec (!= "key" -0.1))
    (!parse ("--key" "-.2e+2") spec (!= "key" -20))))

(defun tuple-spec ()
  (make-arguments-spec "" (:key "key" :type '(tuple integer string float))))

(deftest types-test parsing-tuple ()
  (let ((spec (tuple-spec)))
    (!parse ("--key" "123" "blabla" "1.23") spec
      (!equal "key" '(123 "blabla" 1.23)))))

(deftest types-test non-enough-arguments-for-tuple ()
  (let ((spec (tuple-spec)))
    (!condition (parse-arguments '("--key" "123" "bla") spec)
		missed-key-value-error
		(cmd-parsing-error-argument "key")
		(missed-key-value-error-type 'float))))

(deftest types-test error-parsing-tuple-argument ()
  (let ((spec (tuple-spec)))
    (!condition (parse-arguments '("--key" "123" "bla" "float") spec)
		wrong-key-value-error 
		(cmd-parsing-error-argument "key")
		(wrong-key-value-error-value "float")
		(wrong-key-value-error-type 'float))))

(deftest types-test tuple-help-message ()
  (let ((spec (tuple-spec)))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "   [ARGS ...]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 30 "Products this help message")
		   '("    --key INTEGER STRING FLOAT" 10 "")
		   ""))))

;;file path
;;directory path
;;existing file path
;;existing directory path
;;creatable file path
;;creatable directory path

;;messages for parsing conditions