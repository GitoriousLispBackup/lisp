(in-package :burning-reports)

(defclass report-iteration ()
  ((name :initarg :name :reader iteration-name)
   (values :initarg :values :reader iteration-values)
   (subphases :initarg :subphases :reader iteration-subphases)))

(defun make-iteration (&key (name nil) (values nil) (subphases nil))
  (make-instance 'report-iteration :name name :values values :subphases subphases))

(defclass report-value ()
  ((name :initarg :name :reader value-name)
   (value :initarg :value :reader value-value)
   (measure :initarg :measure :reader value-measure)))

(defun parse-value (node)
  (let ((name (xml-attribute node "name"))
	(value (xml-attribute node "value"))
	(measure (xml-attribute node "measure")))
    (make-instance 'report-value :name name :value value :measure measure)))

(defun parse-iteration (node)
  (make-iteration :name (xml-attribute node "name")
		  :values (mapcar #'parse-value (xml-find-childs node "value"))
		  :subphases (append (mapcar #'parse-phase (xml-find-childs node "phase"))
				     (mapcar #'parse-loop (xml-find-childs node "loop")))))

(defclass report-phase ()
  ((name :initarg :name :reader phase-name)
   (iterations :initarg :iterations :reader phase-iterations)))

(defun make-phase (&key (name nil) (iterations nil))
  (make-instance 'report-phase :name name :iterations iterations)) 

(defun iteration-time (iteration)
  (value-value (find "time" (iteration-values iteration) :test #'equal :key #'value-name)))

(defun phase-time (phase)
  (apply #'+ (mapcar #'iteration-time (phase-iterations phase))))

(defun parse-phase (node)
  (make-phase :name "" :iterations `(,(parse-iteration node))))

(defun parse-loop (node)
  (make-phase :name (xml-attribute node "name") 
	      :iterations (mapcar #'parse-iteration (xml-find-childs node "iteration"))))

(defun parse-from-xml (xml)
  (parse-phase xml))

(defun parse-from-stream (stream)
  (parse-phase (parse-xml stream)))

(defun print-value (value)
  (format t "~a: ~a (~a)~%" (value-name value) (value-value value) (value-measure value)))

(defun print-iteration (iteration)
  (format t "Iteration: ~a~%" (iteration-name iteration))
  (mapc #'print-value (iteration-values iteration))
  (mapc #'print-phase (iteration-subphases iteration)))

(defun print-phase (phase)
  (format t "Phase: ~a~%" (phase-name phase))
  (mapc #'print-iteration (phase-iterations phase)))

(defun check-value (x y)
  (and (equal (value-name x) (value-name y))
       (equal (value-measure x) (value-measure y))))

(defun check-iteration (iteration master)
  (and (eval (cons 'and
		   (mapcar #'check-value (iteration-values iteration) (iteration-values master))))
       (eval (cons 'and
		   (mapcar #'(lambda (x y) (equal (phase-name x) (phase-name y))) 
			   (iteration-subphases iteration) (iteration-subphases master))))))

(defun check-phase (phase)
  (eval (cons 'and
	      (mapcar #'(lambda (x) (check-iteration x (first (phase-iterations phase)))) 
		      (phase-iterations phase)))))

(defun iteration-header (iteration)
  (concatenate 'array
	       (map 'array #'value-name (iteration-values iteration))
	       (map 'array  #'(lambda (x) (concatenate 'string 
						      (phase-name x)
						      "-time"))
		    (iteration-subphases iteration))))

(defun iteration-line (iteration)
  (concatenate 'array
	       (map 'array #'value-value (iteration-values iteration))
	       (map 'array #'phase-time (iteration-subphases iteration))))

(defclass report-table ()
  ((name :initarg :name :reader table-name)
   (header :initarg :header :reader table-header)
   (lines :initarg :lines :reader table-lines)))

(defun concatenate-path (prefix name)
  (cond 
    ((or (equal name "") (not name)) prefix)
    ((equal prefix "") name)
    (t (format nil "~a/~a" prefix name))))

(defun phase-table (phase &optional (name-prefix ""))
  (make-instance 'report-table
		 :name (concatenate-path name-prefix (phase-name phase))
		 :header (iteration-header (first (phase-iterations phase)))
		 :lines (map 'array #'iteration-line (phase-iterations phase))))

(defun report-tables (report)
  (labels 
      ((phase-tables (phase &optional (prefix ""))
	 (cons (phase-table phase prefix)
	       (apply #'append
		      (mapcar #'(lambda (x) (iteration-tables x (concatenate-path prefix (phase-name phase))))
			      (phase-iterations phase)))))
       (iteration-tables (iteration &optional (prefix ""))
	 (apply #'append (mapcar #'(lambda (x) (phase-tables x (concatenate-path prefix
										 (iteration-name iteration))))
				 (iteration-subphases iteration)))))
    (phase-tables report)))

(defun print-with-padding (string size)
  (let ((string (format nil "~a" string)))
    (let* ((before-padding (floor (/ (- size (length string)) 2)))
	   (after-padding (- size before-padding)))
      (format t "~va~va" before-padding "" after-padding string))))

(defun print-table (table)
  (format t "~a:~%" (table-name table))
  (labels ((element-length (array i)
	     (length (format nil "~a" (aref array i))))
	   (print-line (line paddings)
	     (map 'array #'print-with-padding line paddings)
	     (format t "~%")))
    (let ((column-paddings (make-array (length (table-header table)))))
      (dotimes (i (length (table-header table)))
	(setf (aref column-paddings i) (+ 2 (apply #'max
						   (cons (element-length (table-header table) i)
							 (map 'list #'(lambda (x) (element-length x i))
							      (table-lines table)))))))
      (print-line (table-header table) column-paddings)
      (map 'array #'(lambda (x) (print-line x column-paddings)) (table-lines table)))))