(in-package #:burning-command-line-test)

(defcase base-test)

(deftest base-test simple-list ()
  (let ((args (make-argument-list (make-argument "arg1") (make-argument "arg2"))))
    (!t (have-argument-p args "arg1"))
    (!t (have-argument-p args "arg2"))
    (!null (argument-set-p args "arg1"))
    (!null (argument-set-p args "arg2"))))

(deftest base-test simple-flag ()
  (let ((args (make-argument-list)))
    (add-arguments args (make-argument "flag") (make-argument "other flag"))
    (!t (have-argument-p args "flag"))
    (!t (have-argument-p args "other flag"))
    (!null (argument-set-p args "flag"))
    (!null (argument-set-p args "other flag"))))

(deftest base-test non-existing-argument ()
  (let ((args (make-argument-list)))
    (!null (have-argument-p args "some flag"))
    (!condition (argument-set-p args "some flag") argument-not-defined-error
		(argument-not-defined-error-name "some flag"))))

(deftest base-test argument-with-description ()
  (let ((args (make-argument-list)))
    (add-argument args (make-argument "flag" :description "a test flag"))
    (!equal (argument-description (argument args "flag")) "a test flag")))

(deftest base-test adding-flag-twice ()
  (let ((args (make-argument-list (make-argument "flag"))))
    (!condition (add-argument args (make-argument "flag" :description "a flag twice"))
		argument-already-exists-error 
		(argument-already-exists-error-name "flag"))))
		
    