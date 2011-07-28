(load "testing.lisp")

(defcase test-testing
  (true)
  (report)
  (another)
  (=-test)
  (<>-test))
 
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
  (!= (* 2 2) 4))

(deftest test-testing <>-test ()
  (!<> 2 3))