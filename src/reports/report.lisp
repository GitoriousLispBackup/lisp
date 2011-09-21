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

(defun iteration-time-measure (iteration)
  (value-measure (find "time" (iteration-values iteration) :test #'equal :key #'value-name)))

(defun phase-time (phase)
  (apply #'+ (mapcar #'iteration-time (phase-iterations phase))))

(defun phase-time-measure (phase)
  (iteration-time-measure (first (phase-iterations phase))))

(defun parse-phase (node)
  (let ((iteration (parse-iteration node)))
    (setf (slot-value iteration 'name) nil)
    (make-phase :name (xml-attribute node "name") :iterations `(,iteration))))

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
  (labels ((full-value-name (name measure)
	     (if measure
		 (format nil "~a (~a)" name measure)
		 (format nil "~a" name))))
  (concatenate 'array
	       (map 'array #'(lambda (x) (full-value-name (value-name x) (value-measure x)))
		    (iteration-values iteration))
	       (map 'array  #'(lambda (x) (full-value-name (concatenate 'string (phase-name x) "-time") 
							   (phase-time-measure x)))
		    (iteration-subphases iteration)))))

(defun iteration-line (iteration)
  (concatenate 'array
	       (map 'array #'value-value (iteration-values iteration))
	       (map 'array #'phase-time (iteration-subphases iteration))))

(defclass report-table ()
  ((path :initarg :path :reader table-path)
   (header :initarg :header :reader table-header)
   (lines :initarg :lines :reader table-lines)))

(defun cons-last (list atom)
  (if atom
      (append list (list atom))
      list))

(defun phase-table (phase &optional (name-prefix ()))
  (make-instance 'report-table
		 :path (cons-last name-prefix (phase-name phase))
		 :header (iteration-header (first (phase-iterations phase)))
		 :lines (map 'array #'iteration-line (phase-iterations phase))))

(defun print-with-padding (string size)
  (let ((string (format nil "~a" string)))
    (let* ((before-padding (floor (/ (- size (length string)) 2)))
	   (after-padding (- size before-padding)))
      (format t "~va~va" before-padding "" after-padding string))))

(defun print-table (table)
  (format t "~a:~%" (table-path table))
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
      (map 'array #'(lambda (x) (print-line x column-paddings)) (table-lines table))
      t)))

(defun phase-goto (phase next)
  (if (and (= (length (phase-iterations phase)) 1)
	   (null (iteration-name (first (phase-iterations phase)))))
      (iteration-goto (first (phase-iterations phase)) next)
      (if (null next)
	  phase
	  (let ((next-iteration (find (first next) (phase-iterations phase) :test #'equal :key #'iteration-name)))
	    (assert next-iteration)
	    (iteration-goto next-iteration (rest next))))))

(defun iteration-goto (iteration next)
  (if (null next)
      iteration
      (let ((next-phase (find (first next) (iteration-subphases iteration) :test #'equal :key #'phase-name)))
	(assert next-phase)
	(phase-goto next-phase (rest next)))))

(defgeneric ls (object))

(defun print-name (value name-function)
  (format t "~a~%" (funcall name-function value)))

(defmethod ls ((phase report-phase))
  (progn
    (mapc #'(lambda (x) (print-name x #'iteration-name)) (phase-iterations phase))
    t))

(defmethod ls ((iteration report-iteration))
  (progn
    (mapc #'(lambda (x) (print-name x #'phase-name)) (iteration-subphases iteration))
    t))

(defun ls-path (root path)
  (ls (phase-goto root path)))

(defmacro move-path (path &optional (next nil))
  (if next
      `(setf ,path (cons-last ,path ,next))
      `(setf ,path (butlast ,path))))

(defun path-table (root path)
  (phase-table (phase-goto root path)))
  