(in-package #:burning-command-line-test)

(defcase types-test)

(defmacro !parse ((&rest args) spec &body values)
  (let ((args-sym (gensym)))
    (flet ((value-check (value)
	     (destructuring-bind (test name value) value
	       `(,test (argument-value ,name ,args-sym) ,value))))
      `(let ((,args-sym (parse-arguments ',args ,spec)))
	 (declare (ignorable ,args-sym))
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

(defmacro def-fs-type-test (name &body body)
  `(deftest types-test ,name ()
     (let* ((fs (make-virtual-filesystem))
	    (*default-filesystem* fs))
       ,@body)))

(def-fs-type-test file-path-argument
  (let ((spec (make-arguments-spec "" (:key "path" :type 'file-path))))
    (!parse ("--path" "/bla/blabla/bla.ext") spec
      (!equalp "path" (path-from-string "/bla/blabla/bla.ext")))
    (!condition (parse-arguments '("--path" "/bla/") spec)
		wrong-file-path-argument-error
		(wrong-key-value-error-value "/bla/")
		(wrong-key-value-error-type 'file-path)
		(cmd-parsing-error-argument "path"))))

(def-fs-type-test directory-path-argument
  (let ((spec (make-arguments-spec "" (:key "path" :type 'directory-path))))
    (!parse ("--path" "/bla/") spec
      (!equalp "path" (path-from-string "/bla/")))
    (!parse ("--path" "/bla") spec
      (!equalp "path" (path-from-string "/bla/")))))

(def-fs-type-test existing-path-test
  (make-file (path-from-string "/bla/blabla/bla.bla") :recursive t)
  (make-directory (path-from-string "/bla/blabla/bla/") :recursive t)
  (let ((spec (make-arguments-spec "" (:key "path" :type 'existing-file-path))))
    (!parse ("--path" "/bla/blabla/bla.bla") spec
      (!equalp "path" (path-from-string "/bla/blabla/bla.bla")))
    (!condition (parse-arguments '("--path" "/bla/blabla/bla/") spec)
		wrong-file-path-argument-error)
    (!condition (parse-arguments '("--path" "/bla/blabla/bla") spec)
		wrong-file-path-argument-error))
  (let ((spec (make-arguments-spec "" (:key "path" :type 'existing-directory-path))))
    (!parse ("--path" "/bla/blabla/bla/") spec
      (!equalp "path" (path-from-string "/bla/blabla/bla/")))
    (!condition (parse-arguments '("--path" "/bla/blabla/bla.bla") spec)
		wrong-directory-path-argument-error)))

(def-fs-type-test creatable-path-test
  (make-file (path-from-string "a.file"))
  (make-directory (path-from-string "a.directory/" ))
  (let ((spec (make-arguments-spec "" (:key "path" :type 'creatable-file-path))))
    (!parse ("--path" "a.file") spec
      (!equalp "path" (path-from-string "a.file")))
    (!parse ("--path" "a.directory/a.new.file") spec
      (!equalp "path" (path-from-string "a.directory/a.new.file")))
    (!condition (parse-arguments '("--path" "a.directory") spec)
		wrong-file-path-argument-error)
    (!condition (parse-arguments '("--path" "wrong.directory/file") spec)
		wrong-file-path-argument-error))
  (let ((spec (make-arguments-spec "" (:key "path" :type 'creatable-directory-path))))
    (!parse ("--path" "a.directory") spec
      (!equalp "path" (path-from-string "a.directory/")))
    (!parse ("--path" "a.directory/a.subdirectory") spec
      (!equalp "path" (path-from-string "a.directory/a.subdirectory/")))
    (!condition (parse-arguments '("--path" "a.file/") spec)
		wrong-directory-path-argument-error)
    (!condition (parse-arguments '("--path" "wrong.dir/a.dir/") spec)
		wrong-directory-path-argument-error)))

(defmacro !error-message ((err &rest arguments) message)
  `(!equal (cmd-parsing-error-message (make-instance ',err ,@arguments))
	   ,message))

(deftest types-test parsing-error-messages ()
  (!error-message (cmd-parsing-error :argument "arg") "Error parsing argument arg")
  (!error-message (wrong-argument-error :argument "ARG") "Wrong argument name: ARG")
  (!error-message (wrong-short-argument-error :argument #\a) "Wrong argument short name: a")
  (!error-message (wrong-key-value-error :argument "Key" :value "bla" :type 'bad-type)
		  "Wrong value for key Key of type BAD-TYPE: bla")
  (!error-message (missed-key-value-error :argument "AKey" :type 'very-strange)
		  "Missed value for key AKey of type VERY-STRANGE")
  (!error-message (too-few-arguments-in-group-set :argument "Group")
		  "Too few arguments in group Group set")
  (!error-message (too-much-arguments-in-group-set :argument "GRP" :arguments '("ArgA" "ArgB" "ArgC"))
		  "Too much arguments in group GRP set: ArgA ArgB ArgC")
  (!error-message (argument-value-too-low-error :argument "Arg" :type 'an-int :value 0 :min-value 100)
		  "Value for argument Arg of type AN-INT - 0, that is less then 100")
  (!error-message (argument-value-too-high-error :argument "AnArg" :type 'some-type :value 100 :max-value 0)
		  "Value for argument AnArg of type SOME-TYPE - 100, that is more then 0")
  (!error-message (wrong-file-path-argument-error :argument "Path" :type 'a-file-type :value "a path")
		  "Error parsing argument Path: a path isn't correct file path")
  (!error-message (wrong-directory-path-argument-error :argument "DPath" :type 'a-dir-type :value "a dir path")
		  "Error parsing argument DPath: a dir path isn't correct directory path"))

