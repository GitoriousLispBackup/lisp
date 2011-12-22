(in-package #:burning-command-line-test)

(defcase types-test)

(deftest types-test integer-type-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'integer))))
    (let ((args (parse-arguments '("--key" "12345") spec)))
      (!= (argument-value "key" args) 12345))))

(deftest types-test bounded-integers-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type '(integer -100 1000)))))
    (let ((args (parse-arguments '("--key" "123") spec)))
      (!= (argument-value "key" args) 123))
    (!condition (parse-arguments '("--key" "-101") spec)
		argument-value-too-low-error
		(cmd-parsing-error-argument "key")
		(wrong-key-value-error-value -101)
		(argument-value-too-low-error-min-value -100))
    (!condition (parse-arguments '("--key" "1001") spec)
		argument-value-too-high-error
		(cmd-parsing-error-argument "key")
		(wrong-key-value-error-value 1001)
		(argument-value-too-high-error-max-value 1000))))

(deftest types-test string-keys-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'string))))
    (let ((args (parse-arguments '("--key" "blabla") spec)))
      (!equal (argument-value "key" args) "blabla"))))

;;float
;;negative-float
;;negative-float-with-dot

;;tuples

;;paths

;;messages for parsing conditions