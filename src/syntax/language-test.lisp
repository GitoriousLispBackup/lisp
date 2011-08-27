(in-package :burning-syntax-test)

(defcase language-test)

(defun !rule= (rule result &rest productions)
  (!eq (rule-result rule) result)
  (let ((real-productions (rule-productions rule)))
    (!= (length real-productions) (length productions))
    (mapc #'(lambda (x y) (!equal x y)) real-productions productions)))

(deftest language-test simple-rule ()
  (!rule= (make-rule 'simple 'simple1 'simple2) 
	  'simple '(simple1 simple2)))

(deftest language-test or-rule ()
  (!rule= (make-rule 'sample '(|| sample1 sample2) 'sample3 '(|| sample4 sample5))
	  'sample
	  '(sample1 sample3 sample4)
	  '(sample1 sample3 sample5)
	  '(sample2 sample3 sample4)
	  '(sample2 sample3 sample5)))


