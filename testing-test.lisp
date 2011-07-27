(load "testing.lisp")

(defun test-testing ()
  (true)
  (report)
  (another)
  (=-test)
  (format t "~%")
  (format t (get-output-stream-string *summary-stream*)))

(deftest test-testing true ()
  (!t t))

(deftest test-testing report ()
  (!t (= 2 2))
  (!t (= 2 3)))

(deftest test-testing another ()
  (!t (= 2 2))
  (!t (= 2 2))
  (!t (= 2 4))
  (!t (= 2 2))
  (!t (= 2 2)))

(deftest test-testing =-test ()
  (!= (* 2 2) 4)
  (!= (* 2 2) 5))