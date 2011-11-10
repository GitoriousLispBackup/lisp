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

(deftest test-testing condition-test ()
  (!condition (error "bla bla") simple-error))

(deftest test-testing failed-condition-test ()
  (!condition (+ 2 2) simple-error))

(deftest test-testing condition-safe-test ()
  (!condition-safe (+ 2 2)))

(deftest test-testing failed-condition-safe-test ()
  (!condition-safe (error "Not safe")))

(deftest test-testing condition-argument-test ()
  (!condition (error "bla") simple-error 
	      (simple-condition-format-arguments "bla")))