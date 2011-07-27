(defparameter *summary-stream* (make-string-output-stream))
(defparameter *summary-listener* (lambda (report) (format *summary-stream* "~a~%" report)))

(defun report-success (report log-stream)
  (funcall *summary-listener* (concatenate 'string report " success.")))

(defun report-failure (report log-stream)
  (funcall *summary-listener* (concatenate 'string report " FAILED.")))

(defun report-result( expr report &optional (log-stream t))
  (cond ((null expr) (report-failure report log-stream))
        (t (report-success report log-stream))))

(defvar *test-result* t)

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