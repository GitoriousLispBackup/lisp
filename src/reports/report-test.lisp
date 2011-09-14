(in-package :burning-reports-test)

(defcase report-test)

(defun !value= (value1 value2)
  (!equal (burning-reports::value-name value1) (burning-reports::value-name value2))
  (!equal (burning-reports::value-value value1) (burning-reports::value-value value2))
  (!equal (burning-reports::value-measure value1) (burning-reports::value-measure value2)))

(defun !iteration= (iteration1 iteration2)
  (mapc #'!value= (burning-reports::iteration-values iteration1) (burning-reports::iteration-values iteration2)))

(defun !phase= (phase1 phase2)
  (mapc #'!iteration= 
	(burning-reports::phase-iterations phase1)
	(burning-reports::phase-iterations phase2)))

(deftest report-test simple-phase ()
  (let ((xml (make-xml-node "simple-phase")))
    (!phase= (parse-phase xml)
	     (make-phase :iterations `(,(make-iteration))))))

(deftest report-test simple-loop ()
  (let ((xml (make-xml-node 
	      "simple-loop" () 
	      (list (make-xml-node "iteration")
		    (make-xml-node "iteration")
		    (make-xml-node "iteration")))))
    (!phase= (parse-phase xml)
	     (make-phase :iterations (list (make-iteration)
					   (make-iteration)
					   (make-iteration))))))

(deftest report-test value-parse ()
  (let ((xml (make-xml-node "iteration" ()
			    (list (make-xml-node "value" 
						 '(("name" "value1") ("measure" "ms") ("value" 123)))
				  (make-xml-node "value" 
						 '(("name" "value2") ("measure" "m2") ("value" "bla-bla")))))))
    (!iteration= (burning-reports::parse-iteration xml)
		 (burning-reports::make-iteration 
		  :values (list (make-instance 'burning-reports::report-value
					       :name "value1" :value 123 :measure "ms")
				(make-instance 'burning-reports::report-value
					       :name "value2" :value "bla-bla" :measure "m2"))))))
			    
