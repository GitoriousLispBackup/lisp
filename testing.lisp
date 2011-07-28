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

(defmacro defcase (name &body body)
  (progn
   `(setf (get ',name 'tests) ',body)
   `(defun ,name ()
      (setq *success-tests* 0)
      (setq *failed-tests* 0)
      ,@body
      (format t "~%")
      (format t (get-output-stream-string *summary-stream*))
      (format t "~%~a tests of ~a failed.~%" *failed-tests* (+ *failed-tests* *success-tests*)))))


(defmacro deftest (test-case name args &body body)
  `(defun ,name ,args
     (format t "Running test ~a.~a~%" ',test-case ',name)
     (let ((*test-result* t))
       ,@body
       (report-result *test-result* ,(concatenate 'string "Test " (string test-case) "." (string name)))
       (cond ((not (null *test-result*)) (format t "OK.~%"))))))

(defmacro check (check-expr &body body)
  `(if (not ,check-expr)
       (progn (setq *test-result* nil)
	      ,@body)))

(defmacro !t (expr) 
  `(check ,expr (format t "~a is nil.~%" ',expr)))

(defmacro != (expr1 expr2)
  `(check (= ,expr1 ,expr2)
     (format t "~a is ~a.Expected ~a which is ~a.~%" ',expr1 ,expr1 ',expr2 ,expr2 )))

(defmacro !<> (expr1 expr2)
  `(check (/= ,expr1 ,expr2)
     (format t "~a is ~a. Expected not ~a.~%" ',expr1 ,expr1 ',expr2))) 