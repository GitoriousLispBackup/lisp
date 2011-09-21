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

(defun make-value (name value measure)
  (make-instance 'burning-reports::report-value :name name :value value :measure measure))

(deftest report-test value-parse ()
  (let ((xml (make-xml-node "iteration" ()
			    (list (make-xml-node "value" 
						 '(("name" "value1") ("measure" "ms") ("value" 123)))
				  (make-xml-node "value" 
						 '(("name" "value2") ("measure" "m2") ("value" "bla-bla")))))))
    (!iteration= (burning-reports::parse-iteration xml)
		 (make-iteration 
		  :values (list (make-value "value1" 123 "ms")
				(make-value "value2" "bla-bla" "m2"))))))

(deftest report-test time-test ()
  (let ((phase (make-phase :iterations (list (make-iteration :values `(,(make-value "time" 12 "ms")))
					     (make-iteration :values `(,(make-value "time" 13 "ms")))))))
    (!= (burning-reports::phase-time phase) 25)
    (!equal (burning-reports::phase-time-measure phase) "ms")))

(deftest report-test phase-goto-test ()
  (let ((subphase1 (make-phase :name "subphase1"))
	(subphase2 (make-phase :name "subphase2")))
    (let ((iteration1 (make-iteration :name "iteration1" 
				      :subphases `(,subphase1)))
	  (iteration2 (make-iteration :name "iteration2"
				      :subphases `(,subphase2))))
      (let ((phase (make-phase :iterations `(,iteration1 ,iteration2))))
	(!iteration= (phase-goto phase '("iteration1")) iteration1)
	(!iteration= (phase-goto phase '("iteration2")) iteration2)
	(!phase= (phase-goto phase '("iteration1" "subphase1")) subphase1)
	(!phase= (phase-goto phase '("iteration2" "subphase2")) subphase2)))))

(defun make-table (path header &rest lines)
  (make-instance 'burning-reports::report-table
		 :path path
		 :header (coerce header 'array)
		 :lines (map 'array #'(lambda (x) (coerce x 'array)) lines)))

(defun !table= (table1 table2)
  (!equal (burning-reports::table-path table1) (burning-reports::table-path table2))
  (!equalp (burning-reports::table-header table1) (burning-reports::table-header table2))
  (!equalp (burning-reports::table-lines table1) (burning-reports::table-lines table2)))

(deftest report-test phase-table-test ()
  (let ((iteration1 (make-iteration :name "iteration1"
				    :values `(,(make-value "value1" 1 "m")
					       ,(make-value "value2" 2 "m2"))))
	(iteration2 (make-iteration :name "iteration2"
				    :values `(,(make-value "value1" "10" "m")
					       ,(make-value "value2" "20" "m2")))))
    (let ((phase (make-phase :name "phase" :iterations `(,iteration1 ,iteration2))))
      (!table= (phase-table phase)
	       (make-table '("phase")
			   '("value1 (m)" "value2 (m2)")
			   '(1 2) '("10" "20"))))))
			   
			   

									      
