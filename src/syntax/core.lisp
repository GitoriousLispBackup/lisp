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
   (start-symbol :initarg :start-symbol :reader grammar-start)
   (terminals :initarg :terminals :reader grammar-terminals)
   (non-terminals :initarg :non-terminals :reader grammar-non-terminals)))

(defun terminal-p (symbol grammar)
  (find symbol (grammar-terminals grammar)))

(defun symbol-productions (symbol grammar)
  (remove-if-not #'(lambda (x) (eq (first x) symbol)) (grammar-productions grammar)))

(defun multi-union (lists)
  (cond 
    ((null lists) ())
    ((null (rest lists)) (first lists))
    (t (union (first lists) (multi-union (rest lists))))))

(defun make-grammar (rules &key (start-symbol))
  (let* ((productions (apply #'append (mapcar #'rule-to-productions rules)))
	 (terminals (multi-union (mapcar #'production-terminals productions)))
	 (non-terminals (multi-union (mapcar #'production-non-terminals productions))))
    (make-instance 'grammar
		   :productions productions
		   :start-symbol start-symbol
		   :terminals (cons :eps terminals)
		   :non-terminals non-terminals)))

(defun nullable-p (symbol grammar &optional (deny-list ()))
  (cond 
    ((terminal-p symbol grammar) nil)
    ((find symbol deny-list) nil)
    (t (eval (cons 'or (mapcar #'(lambda (x) (nullable-production-p x grammar (cons symbol deny-list)))
			       (symbol-productions symbol grammar)))))))

(defun nullable-production-p (production grammar &optional (deny-list ()))
  (eval (cons 'and (mapcar #'(lambda (x) (nullable-p x grammar deny-list)) (rest production)))))
  
(defun symbol-first (symbol grammar &optional (deny-list ()))
  (cond
    ((terminal-p symbol grammar) `(,symbol))
    ((find symbol deny-list) nil)
    (t (multi-union (mapcar #'(lambda (x) (production-first (rest x) grammar (cons symbol deny-list))) 
			    (symbol-productions symbol grammar))))))

(defun production-first (production grammar &optional (deny-list ()))
  (cond
    ((null production) nil)
    ((not (nullable-p (first production) grammar)) (symbol-first (first production) grammar deny-list))
    (t (union (symbol-first (first production) grammar deny-list) 
	      (production-first (rest production) grammar deny-list)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defined-production (expr)
    (let ((length (length expr)))
      (when (> length 2)
	(error "Too much parameters for production definition. Only 2 expected."))
      (when (< length 2)
	(error "Not enoght parameters for production definition. 2 expected.")))
    (let ((production (second expr)))
      (if (atom production)
	  `(make-rule ',(first expr) ',production)
	  `(make-rule ',(first expr) ,@(mapcar #'(lambda (x) `',x) production))))))

(defmacro defgrammar (name (&rest productions) &key (start))
  `(defparameter ,name 
     (make-grammar (list ,@(mapcar #'(lambda (x) (defined-production x)) productions))
		   :start-symbol ',start)))

    
  