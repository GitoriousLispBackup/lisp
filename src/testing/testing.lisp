
(defpackage :burning-testing
  (:use common-lisp)
  (:export :defcase
	   :deftest
	   :!t
	   :!not
	   :!null
	   :!=
	   :equal-check
	   :!eq
	   :!equal
	   :!equalp
	   :!<>
	   :!condition
	   :!condition-safe
	   :!error
	   :run-test
	   :run-case
	   :run-tests))

(in-package :burning-testing)

(defparameter *test-packages* ())

(defparameter *summary-stream* (make-string-output-stream))
(defparameter *summary-listener* (lambda (report) (format *summary-stream* "~a~%" report)))

(defparameter *success-tests* 0)
(defparameter *failed-tests* 0)

(defun report-success (report)
  (funcall *summary-listener* (concatenate 'string report " success."))
  (incf *success-tests*))

(defun report-failure (report)
  (funcall *summary-listener* (concatenate 'string report " FAILED."))
  (incf *failed-tests*))

(defun report-result( expr report)
  (cond ((null expr) (report-failure report))
        (t (report-success report))))

(defvar *test-result* t)

(defparameter *test-cases* ())

(defmacro defcase (name)
  `(progn 
     (setq *test-cases* (union *test-cases* (list ',name)))
     (setf (get ',name 'tests) nil)
     (defclass ,name () nil)))

(defmacro deftest (test-case name args &body body)
  `(progn
     (setf (get ',test-case 'tests) (union (get ',test-case 'tests) (list ',name)))
     (defmethod ,name ((a-case ,test-case ) ,@args)
       (format t "Running test ~a.~a~%" ',test-case ',name)
       (let ((*test-result* t))
	 ,@body
	 (report-result *test-result* ,(concatenate 'string "Test " (string test-case) "." (string name)))
	 (cond ((not (null *test-result*)) (format t "OK.~%")))))))

(defmacro check (check-expr &body body)
  `(if (not ,check-expr)
       (progn (setq *test-result* nil)
	      ,@body)))

(defmacro equal-check (expr1 expr2 predicate)
  (let ((value1 (gensym)) (value2 (gensym)))
    `(let ((,value1 ,expr1) (,value2 ,expr2))
       (check (,predicate ,value1 ,value2)
	      (format t "~a is ~a.Expected ~a which is ~a.~%" ',expr1 ,value1 ',expr2 ,value2)))))

(defmacro !t (expr) 
  `(check ,expr (format t "~a is nil.~%" ',expr)))

(defmacro !not (expr)
  `(!null ,expr))

(defmacro !null (expr)
  `(check (not ,expr) (format t "~a is not nil.~%" ',expr)))

(defmacro != (expr1 expr2)
  `(equal-check ,expr1 ,expr2 =))

(defmacro !equal (expr1 expr2)
  `(equal-check ,expr1 ,expr2 equal))

(defmacro !equalp (expr1 expr2)
  `(equal-check ,expr1 ,expr2 equalp))

(defmacro !eq (expr1 expr2)
  `(equal-check ,expr1 ,expr2 eq))

(defmacro !<> (expr1 expr2)
  (let ((value1 (gensym)) (value2 (gensym)))
    `(let ((,value1 ,expr1) (,value2 ,expr2))
       (check (/= ,value1 ,value2)
	 (format t "~a is ~a. Expected not ~a.~%" ',expr1 ,value1 ',expr2)))))

(defun message (error)
  (let ((str (make-array 0 :element-type 'base-char
			 :fill-pointer 0 :adjustable t)))
    (with-output-to-string (output str)
      (apply #'format (append (list output (simple-condition-format-control error))
			      (simple-condition-format-arguments error)))
      str)))
	    
(defmacro !error (expression message)
  `(handler-case
       (progn ,expression
	      (check nil (format t "~a failed to die.~%" ',expression)))
     (simple-error (error)
       (!equal (message error) ,message))))

(defmacro !condition (expression condition &rest arg-forms)
  (let ((condition-sym (gensym)))
    (labels ((do-check-form (reader value &key (test 'eql))
	     `(,test (,reader ,condition-sym) ,value))
	     (check-form (form)
	       (apply #'do-check-form form)))
      `(handler-case
	   (progn ,expression
		  (check nil (format t "~a failed to die.~%" ',expression)))
	 (,condition (,condition-sym)
	   ,@(mapcar #'check-form arg-forms))))))

(defmacro !condition-safe (expression)
  `(handler-case
       ,expression
     (error (err)
       (check nil (format t "Died with error ~a.~%" err)))))

(defun prepare-test ()
  (setq *success-tests* 0)
  (setq *failed-tests* 0))

(defun test-summary ()
  (format t "~%")
  (format t (get-output-stream-string *summary-stream*))
  (format t "~%~a tests of ~a success.~%" *success-tests* (+ *failed-tests* *success-tests*)))

(defun do-run-test (name case)
  (funcall name (make-instance case)))

(defun run-test (name case-name)
  (prepare-test)
  (do-run-test name case-name)
  (test-summary))

(defun do-run-case (name)
  (mapcar #'(lambda (test) (do-run-test test name)) (get name 'tests)))

(defun run-case (name)
  (prepare-test)
  (do-run-case name)
  (test-summary))

(defun run-tests ()
  (prepare-test)
  (dolist (test-case *test-cases*)
    (do-run-case test-case))
  (test-summary))
