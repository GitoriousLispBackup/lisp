(in-package :burning-lexical)

(defclass state-machine () 
  ((translation :initarg :translation)
   (values :initarg :values 
	   :initform (make-array 0 :adjustable t :fill-pointer 0))
   (transitions :initarg :transitions 
		:initform (make-array 0 :adjustable t :fill-pointer 0))))

(defgeneric (setf translation) (value machine))
(defgeneric add-state (machine))
(defgeneric value (machine state))
(defgeneric (setf value) (value machine state))
(defgeneric next (machine state code))
(defgeneric (setf next) (value machine state code))
(defgeneric first-state (machine))

(defmethod first-state ((machine state-machine))
  0)

(defmethod fill-translation (translation first last value)
  (do ((i first (1+ i)))
      ((> i last) t)
    (setf (elt translation i) value)))

(defmethod (setf translation) (value (machine state-machine))    
  (setf (slot-value machine 'translation)
	(make-array (1+ (char-code (cdaar (last value)))) :initial-element -1))
  (dolist (range value)
    (fill-translation (slot-value machine 'translation)
		      (char-code (caar range))
		      (char-code (cdar range))
		      (second range))))

(defmethod add-state ((machine state-machine))
  (with-slots (values transitions) machine
    (vector-push-extend nil values)
    (vector-push-extend nil transitions)
    (- (length values) 1)))

(defmethod value ((machine state-machine) state)
  (elt (slot-value machine 'values) state))

(defmethod (setf value) (value (machine state-machine) state)
  (setf (elt (slot-value machine 'values) state)
	value))

(defmethod next ((machine state-machine) state code)
  (if (null code)
      nil
      (cdr (assoc (elt (slot-value machine 'translation) (char-code code))
		  (elt (slot-value machine 'transitions) 
		       state)))))

(defmethod (setf next) (value (machine state-machine) state code)
  (push (cons code value)
	(elt (slot-value machine 'transitions) state)))

(defun fill-nexts (state lexic table)
  (let ((next (elt (next-vector lexic) state)))
    (unless (null next)
      (setf (gethash next table)
	    (make-set (copy-list (elt (follow-vector lexic) state))
		      (gethash next table))))))

(defun add-state-function (state machine visited-states &aux (new-states nil))
  (list 
   (lambda (next next-state)
     (let ((next-state-id (gethash next-state visited-states)))
       (unless next-state-id
	 (setq next-state-id (add-state machine))
	 (push next-state new-states)
	 (setf (gethash next-state visited-states) next-state-id))
       (setf (next machine 
		   (gethash state visited-states)
		   next) 
	     next-state-id)))
   (lambda () new-states)))

(defun state-value (state-set lexic)
  (dolist (state state-set)
    (let ((value (elt (value-vector lexic) state)))
      (if value
	  (return value)))))

(defun set-state-transitions (state-set lexic machine visited-states)
  (let ((next-states (make-hash-table))
	(add-state-funcs (add-state-function state-set machine visited-states)))
    (dolist (state state-set)
      (fill-nexts state lexic next-states))
    (maphash (first add-state-funcs) next-states)
    (funcall (second add-state-funcs))))

(defun process-state (state lexic machine visited-states &aux next-states)
  (setf (value machine (gethash state visited-states))
	(state-value state lexic))
  (setq next-states (set-state-transitions state lexic machine visited-states))
  (dolist (next-state next-states)
    (process-state next-state lexic machine visited-states)))

(defun create-state-machine (lexic &optional (machine-class 'state-machine))
  (let ((machine (make-instance machine-class))
	(visited-states (make-hash-table :test 'equal))
	(first-state (first-pos (expression lexic))))
    (setf (gethash first-state visited-states) (add-state machine))
    (process-state first-state lexic machine visited-states)
    (setf (translation machine) (translation lexic))
    machine))

(defun machine-value (machine string &optional (state 0) (pos 0))
  (cond 
    ((null state) nil)
     ((= (length string) pos) (value machine state))
     (t (machine-value machine string (next machine state (char string pos))
		       (1+ pos)))))

(defun get-token (iterator machine)
  (declare (optimize (speed 3) (safety 0)))
  (do ((result "")
       (type nil)
       (state (first-state machine))
       (safed-iterator (copy-simple-iterator (back iterator))))
      ((null state) (cond ((equal safed-iterator (back iterator)) (error "Wrong token ~a" (commit iterator)))
			  (t (setf (first iterator) safed-iterator)
			     (setf result (commit iterator))
			     (cons result type))))
    (when (value machine state)
      (setq type (value machine state))
      (setq safed-iterator (copy-simple-iterator (forward iterator))))
    (setq state (next machine state (get-next iterator)))))

(defun print-stream (iterator machine)
  (do ()
      ((eof-p iterator) t)
    (let ((token (get-token iterator machine)))
      ())))
    
  