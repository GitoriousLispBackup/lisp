(in-package :burning-syntax)

(defclass rule ()
  ((productions :initarg :productions :accessor rule-productions)
   (result :initarg :result :reader rule-result)
   (aux-rules :initform () :accessor aux-rules)))

(defun rule-to-productions (rule)
  (append (mapcar #'(lambda (x) (cons (rule-result rule) x)) (rule-productions rule))
	  (apply #'append (mapcar #'rule-to-productions (aux-rules rule)))))

(defun list-to-set (list)
  (cond ((null list) nil)
	(t (adjoin (first list) (list-to-set (rest list))))))

(defun production-non-terminals (production)
  (when (get (first production) 'lexeme)
    (error "Production's result must be non-terminal.")) 
  (list-to-set (cons (first production) 
		     (remove-if #'(lambda (x) (get x 'lexeme)) (rest production)))))

(defun production-terminals (production)
  (list-to-set (remove-if-not #'(lambda (x) (get x 'lexeme)) (rest production))))

(defclass grammar ()
  ((productions :initarg :productions :reader grammar-productions)
   (terminals :initarg :terminals :reader grammar-terminals)
   (non-terminals :initarg :non-terminals :reader grammar-non-terminals)))

(defun multi-union (lists)
  (cond 
    ((null lists) ())
    ((null (rest lists)) (first lists))
    (t (union (first lists) (multi-union (rest lists))))))

(defun make-grammar (&rest rules)
  (let* ((productions (apply #'append (mapcar #'rule-to-productions rules)))
	 (terminals (multi-union (mapcar #'production-terminals productions)))
	 (non-terminals (multi-union (mapcar #'production-non-terminals productions))))
    (make-instance 'grammar
		   :productions productions
		   :terminals terminals
		   :non-terminals non-terminals)))


	       
	 
	 

  