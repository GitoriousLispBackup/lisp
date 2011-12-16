(in-package #:burning-command-line-test)

(defcase base-test)

(deftest base-test simple-list ()
  (let ((args (make-argument-list :arguments (list (make-argument "arg1") (make-argument "arg2")))))
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
  (let ((args (make-argument-list :arguments `(,(make-argument "flag")))))
    (!condition (add-argument args (make-argument "flag" :description "a flag twice"))
		argument-already-exists-error 
		(argument-already-exists-error-name "flag"))))
		
(defun lines (&rest args)
  (labels ((line (arg)
	     (cond 
	       ((atom arg) (format nil "~a~%" arg))
	       ((null (rest arg)) (line (first arg)))
	       (t (concatenate 'string
			       (format nil "~a~va" (first arg) (second arg) "")
			       (line (rest (rest arg))))))))
    (cond
      ((null args) "")
      (t (concatenate 'string (line (first args)) (apply #'lines (rest args)))))))

(deftest base-test help-message-test ()
  (!equal (help-message (make-argument-list))
	  (lines "Usage:"
		 "  [ARGS]"
		 ""
		 "  Where ARGS are:"
		 '("    --help" 10 "Products this help message")
		 "")))

(deftest base-test help-with-flag ()
  (!equal (help-message (make-argument-list :arguments `(,(make-argument "flag" :description "some flag"))))
	  (lines "Usage:"
		 "  [ARGS]"
		 ""
		 "  Where ARGS are:"
		 '("    --help" 10 "Products this help message")
		 '("    --flag" 10 "some flag")
		 "")))

(deftest base-test argument-short-names ()
  (let ((args (make-argument-list :arguments `(,(make-argument "flag" :short #\f)))))
    (!eq (argument-short-name (argument args "flag")) #\f)))

(deftest base-test arguments-with-same-short-names ()
  (let ((args (make-argument-list :arguments `(,(make-argument "arg" :short #\a)))))
    (!condition (add-argument args (make-argument "an arg" :short #\a))
		short-name-already-exists-error
		(short-name-already-exists-error-char #\a))))

(deftest base-test custom-help-flag ()
  (let* ((help-arg (make-argument "help" :short #\h :description "My help message"))
	 (args (make-argument-list :help help-arg)))
    (!t (have-argument-p args "help"))
    (!equalp (argument args "help") help-arg)))

(deftest base-test no-help-flag ()
  (let ((args (make-argument-list :help nil)))
    (!null (have-argument-p args "help"))))

(deftest base-test print-short-names ()
  (let ((args (make-argument-list :arguments `(,(make-argument "flag" :short #\f :description "some flag")))))
    (!equalp (help-message args)
	     (lines "Usage:"
		    "  [ARGS]"
		    ""
		    "  Where ARGS are:"
		    '("    --help" 13 "Products this help message")
		    '("    --flag,-f" 10 "some flag")
		    ""))))

(deftest base-test key-help ()
  (let ((args (make-argument-list 
	       :arguments `(,(make-argument "key" :description "some key" :type 'integer)))))
    (!equal (help-message args)
	    (lines "Usage:"
		   "  [ARGS]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 13 "Products this help message")
		   '("    --key ARG" 10 "some key")
		   ""))))

(deftest base-test list-key-help ()
  (let ((args (make-argument-list
	       :arguments `(,(make-argument "key" :description "some key" :type '(list integer))))))
    (!equal (help-message args)
	    (lines "Usage:"
		   "  [ARGS]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 20 "Products this help message")
		   '("    --key [ARG1 ...]" 10 "some key")
		   ""))))