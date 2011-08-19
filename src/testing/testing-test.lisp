(load "testing.lisp")

(defpackage :burning-testing-test
  (:use :common-lisp
        :burning-testing))

(in-package :burning-testing-test)

(defcase test-testing)

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

(deftest test-testing equal-test ()
  (!equal '(2 2) '(2 2))
  (!equal '((2 2) 2) '((2 2) 2))
  (!equal '(2 2) '(2 2 (2 2))))

(defcase other-testing)

(deftest other-testing true ()
  (!t t))

(deftest test-testing ok-error-test ()
  (!error (error "bla bla") "bla bla"))

(deftest test-testing failed-error-test ()
  (!error (+ 2 2) "some error"))
