(defparameter *summary-stream* (make-string-output-stream))
(defparameter *summary-listener* (lambda (report) (format *summary-stream* "~a~%" report)))

(defparameter *success-tests* 0)
(defparameter *failed-tests* 0)

(defun report-success (report log-stream)
  (funcall *summary-listener* (concatenate 'string report " success."))
  (incf *success-tests*))

(defun report-failure (report log-stream)
  (funcall *summary-listener* (concatenate 'string report " FAILED."))
  (incf *failed-tests*))

(defun report-result( expr report &optional (log-stream t))
  (cond ((null expr) (report-failure report log-stream))
        (t (report-success report log-stream))))

(defvar *test-result* t)

(defmacro defcase (name)
  `(progn 
     (if (boundp '*test-cases*)
	 (setq *test-cases* (adjoin ',name *test-cases*))
	 (setq *test-cases* (list ',name)))
     (defclass ,name () nil)))

(defmacro deftest (test-case name args &body body)
  `(progn
     (setf (get ',test-case 'tests) (adjoin ',name (get ',test-case 'tests)))
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

(defmacro !t (expr) 
  `(check ,expr (format t "~a is nil.~%" ',expr)))

(defmacro != (expr1 expr2)
  (let ((value1 (gensym)) (value2 (gensym)))
    `(let ((,value1 ,expr1) (,value2 ,expr2))
       (check (= ,value1 ,value2)
	 (format t "~a is ~a.Expected ~a which is ~a.~%" ',expr1 ,value1 ',expr2 ,value2)))))

(defmacro !<> (expr1 expr2)
  (let ((value1 (gensym)) (value2 (gensym)))
    `(let ((,value1 ,expr1) (,value2 ,expr2))
       (check (/= ,value1 ,value2)
	 (format t "~a is ~a. Expected not ~a.~%" ',expr1 ,value1 ',expr2)))))

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



