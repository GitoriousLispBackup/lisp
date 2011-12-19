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

(deftest base-test help-flag-parsing ()
  (let ((spec (make-arguments-spec)))
    (let ((args (parse-arguments '("--help") spec)))
      (!t (argument-set-p "help" args)))))

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

(deftest base-test print-short-names ()
  (let ((spec (make-arguments-spec "" (:flag "flag" :short-name #\f :description "some flag"))))
    (!equalp (help-message spec)
	     (lines "Usage:"
		    "   [ARGS]"
		    ""
		    "  Where ARGS are:"
		    '("    --help" 13 "Products this help message")
		    '("    --flag,-f" 10 "some flag")
		    ""))))

;;
;; Key tests
;;

(deftest base-test simple-key-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'integer :description "some key"))))
    (!t (have-argument-p "key" spec))
    (let ((args (parse-arguments '("--key" "123") spec)))
      (!t (argument-set-p "key" args))
      (!= (argument-value "key" args) 123))))

(deftest base-test wrong-value-for-key ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'integer))))
    (!condition (parse-arguments '("--key" "blabla") spec)
		wrong-key-value-error
		(wrong-key-value-error-value "123")
		(wrong-key-value-error-type 'integer))))

(deftest base-test missed-key-value-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type 'integer))))
    (!condition (parse-arguments '("--key") spec)
		missed-key-value-error
		(missed-key-value-error-type 'integer))
    (!condition (parse-arguments '("--key" "--key") spec)
		missed-key-value-error
		(missed-key-value-error-type 'integer))))

(deftest base-test key-help ()
  (let ((spec (make-arguments-spec "Spec" (:key "key" :type 'integer :short-name #\k :description "some key"))))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "  Spec [ARGS]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 20 "Products this help message")
		   '("    --key,-k INTEGER" 10 "some key")
		   ""))))

(deftest base-test list-key-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type '(list integer)))))
    (let ((args (parse-arguments '("--key" "123" "456" "789") spec)))
      (!t (argument-set-p "key" args))
      (!equal (argument-value "key" args) '(123 456 789)))))

(deftest base-test key-after-list-test ()
  (let ((spec (make-arguments-spec "" (:key "key" :type '(list integer)) (:key "key2" :type 'string))))
    (let ((args (parse-arguments '("--key" "123" "456" "--key2" "blabla") spec)))
      (!t (argument-set-p "key" args))
      (!t (argument-set-p "key2" args))
      (!equal (argument-value "key" args) '(123 456))
      (!equal (argument-value "key2" args) "blabla"))))

(deftest base-test list-key-help ()
  (let ((args (make-arguments-spec "SP" (:key "key" :type '(list integer) :description "some key"))))
    (!equal (help-message args)
	    (lines "Usage:"
		   "  SP [ARGS]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 23 "Products this help message")
		   '("    --key [INTEGER ...]" 10 "some key")
		   ""))))

(deftest base-test action-argument-test ()
  (let ((spec (make-arguments-spec "Spec" (:action "act" :arguments ((:flag "flag"))))))
    (!t (have-argument-p "act" spec))
    (!t (have-argument-p "flag" spec))))

(deftest base-test action-help-test ()
  (let ((spec (make-arguments-spec "Spec" (:action "act" :short-name #\a
						   :description "destructs universe"
						   :arguments ((:flag "flag"))))))
    (!equal (help-message (argument "act" spec))
	    (lines "destructs universe"
		   ""
		   "Usage:"
		   "  Spec --act,-a [ARGS]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 10 "Products this help message")
		   '("    --flag" 10 "")
		   ""))))

(deftest base-test spec-with-description-help ()
  (let ((spec (make-arguments-spec ("spec" :description "destructs universe"))))
    (!equal (help-message spec)
	    (lines "destructs universe"
		   ""
		   "Usage:"
		   "  spec [ARGS]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 10 "Products this help message")
		   ""))))

;;Parsing action's arguments
;;Argument for non-set action
;;Subactions same arguments
;;Parsing positionals
;;Positionals in subaction


;;Arguments with subarguments
;;Action group
;;Groups

#|
(deftest base-test positionals-insert ()
  (let ((args (make-argument-list 

;;positional help
|#