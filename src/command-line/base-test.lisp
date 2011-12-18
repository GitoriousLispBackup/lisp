(in-package #:burning-command-line-test)

(defcase base-test)

;;
;; Flags test
;; 

(deftest base-test simple-list ()
  (let ((args (make-arguments-spec "" 
		(:flag "arg1") (:flag "arg2"))))
    (!t (have-argument-p "arg1" args))
    (!t (have-argument-p "arg2" args))))

(deftest base-test non-existing-argument ()
  (let ((spec (make-arguments-spec "")))
    (!null (have-argument-p "some flag" spec))))

(deftest base-test parse-simple-list ()
  (let ((spec (make-arguments-spec "" (:flag "flag")  (:flag "flag2"))))
    (let ((args (parse-arguments '("--flag") spec)))
      (!t (argument-set-p "flag" args))
      (!null (argument-set-p "flag2" args)))))

(deftest base-test parsing-wrong-argument ()
  (let ((args (make-arguments-spec "args" (:flag "flag"))))
    (!condition (parse-arguments '("a-flag") args) 
		wrong-argument-error
		(wrong-argument-error-string "a-flag"))))

(deftest base-test argument-with-description ()
  (let ((spec (make-arguments-spec "" (:flag "flag" :description "a test flag"))))
    (!t (have-argument-p "flag" spec))
    (!equal (argument-description (argument "flag" spec)) "a test flag")))

(deftest base-test argument-with-same-names ()
  (!condition (make-arguments-spec "" (:flag "flag") (:flag "flag"))
	      argument-already-exists-error
	      (argument-already-exists-error-name "flag")))

(deftest base-test adding-arguments ()
  (let ((spec (make-arguments-spec "" (:flag "flag1"))))
    (add-argument (make-argument :flag "flag2") spec)
    (!t (have-argument-p "flag1" spec))
    (!t (have-argument-p "flag2" spec))))
  
(deftest base-test adding-argument-twice ()
  (let ((spec (make-arguments-spec "" (:flag "flag"))))
    (!condition (add-argument (make-argument :flag "flag") spec)
		argument-already-exists-error
		(argument-already-exists-error-name "flag"))))

;;
;; Base help messages test
;;

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
  (!equal (help-message (make-arguments-spec ""))
	  (lines "Usage:"
		 "   [ARGS]"
		 ""
		 "  Where ARGS are:"
		 '("    --help" 10 "Products this help message")
		 "")))

(deftest base-test help-with-flag ()
  (!equal (help-message (make-arguments-spec "SPEC" (:flag "flag" :description "some flag")))
	  (lines "Usage:"
		 "  SPEC [ARGS]"
		 ""
		 "  Where ARGS are:"
		 '("    --help" 10 "Products this help message")
		 '("    --flag" 10 "some flag")
		 "")))

(deftest base-test custom-help-flag ()
  (let ((spec (make-arguments-spec ("" (:help "my-help" :short-name #\m :description "my own help")))))
    (!t (have-argument-p "my-help" spec))
    (!null (have-argument-p "help" spec))
    (!eq (argument-short-name (argument "my-help" spec)) #\m)
    (!equal (argument-description (argument "my-help" spec)) "my own help")))


(deftest base-test no-help-flag ()
  (let ((spec (make-arguments-spec ("" :no-help))))
    (!null (have-argument-p "help" spec))))

;;Help flag parsing

;; Short names test

(deftest base-test making-with-short-names ()
  (let ((spec (make-arguments-spec "" (:flag "flag" :short-name #\a))))
    (!eq (argument-short-name (argument "flag" spec)) #\a)))

(deftest base-test makeing-arguments-with-same-short-names ()
  (!condition (make-arguments-spec "" (:flag "f1" :short-name #\f) (:flag "f2" :short-name #\f))
	      short-name-already-exists-error
	      (short-name-already-exists-error-char #\f)))

(deftest base-test parsing-short-names ()
  (let ((spec (make-arguments-spec "" 
		(:flag "f1" :short-name #\a)
		(:flag "f2" :short-name #\b))))
    (let ((args (parse-arguments '("-a") spec)))
      (!t (argument-set-p "f1" args))
      (!null (argument-set-p "f2" args)))))
				   
(deftest base-test non-existing-short-name ()
  (let ((spec (make-arguments-spec "" (:flag "f1" :short-name #\a))))
    (!condition (parse-arguments '("-b") spec) 
		wrong-short-argument-error
		(wrong-short-argument-error-char #\b))))

(deftest base-test multiple-short-names-parsing ()
  (let ((spec (make-arguments-spec ""
		(:flag "f1" :short-name #\1)
		(:flag "f2" :short-name #\2)
		(:flag "f3" :short-name #\3))))
    (let ((args (parse-arguments '("-13") spec)))
      (!t (argument-set-p "f1" args))
      (!t (argument-set-p "f3" args))
      (!null (argument-set-p "f2" args)))))

;;Simple key
;;Wrong value for key
;;Missed key value
;;List key
;;Flag after vector
;;Parsing action's arguments
;;Argument for non-set action
;;Subactions same arguments
;;Parsing positionals
;;Positionals in subaction


;;Arguments with subarguments
;;Action group
;;Groups

#|

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

(deftest base-test positionals-insert ()
  (let ((args (make-argument-list 

;;positional help
|#