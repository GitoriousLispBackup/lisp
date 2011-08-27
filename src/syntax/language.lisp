(in-package :burning-syntax)

(defclass rule ()
  ((productions :initarg :productions :reader rule-productions)
   (result :initarg :result :reader rule-result)
   (aux-rules :initform () :accessor aux-rules)))

(defun append-production (production productions)
  (mapcar #'(lambda (x) (append production x)) productions))

(defgeneric parse-operation (operation arguments rule))

(defmethod parse-operation ((operation (eql '||)) arguments rule)
  (apply #'append (mapcar #'(lambda (x) (parse-symbol x rule)) arguments)))

(defun parse-symbol (symbol rule)
  (cond
    ((listp symbol) (parse-operation (first symbol) (rest symbol) rule))
    (t (list (list symbol)))))

(defun make-productions (production rule)
  (cond 
    ((null production) ())
    ((null (rest production)) (parse-symbol (first production) rule))
    (t (let ((symbols (parse-symbol (first production) rule))
	     (productions (make-productions (rest production) rule)))
	 (apply #'append (mapcar #'(lambda (x) (append-production x productions)) symbols))))))

(defun make-rule (result &rest production)
  (let ((rule (make-instance 'rule :result result)))
    (setf (slot-value rule 'productions) 
	  (make-productions production rule))
    rule))
