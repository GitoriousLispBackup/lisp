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

(defun make-point (production)
  (list production (rest production) ()))

(defun make-points (symbol grammar)
  (mapcar #'make-point (symbol-productions symbol grammar)))

(defun point-production (point)
  (first point))

(defun point-position (point)
  (second point))

(defun point-lookups (point)
  (third point))

(defun symbol< (symbol1 symbol2)
  (string< (symbol-name symbol1) (symbol-name symbol2)))

(defun production< (production1 production2)
  (cond
    ((null production1) t)
    ((null production2) nil)
    ((symbol< (first production1) (first production2)) t)
    ((symbol< (first production2) (first production1)) nil)
    (t (production< (rest production1) (rest production2)))))

(defun point< (point1 point2)
  (cond
    ((production< (point-production point1) (point-production point2)) t)
    ((production< (point-production point2) (point-production point1)) nil)
    (t (> (length (point-position point1)) (length (point-position point2))))))

(defun symbol-closure (symbol grammar symbols-list)
  (cond
    ((terminal-p symbol grammar) symbols-list)
    ((find symbol symbols-list) symbols-list)
    (t (sort (adjoin symbol 
		     (multi-union (mapcar #'(lambda (x) (point-closure x grammar (cons symbol symbols-list))) 
					  (make-points symbol grammar))))
		   #'symbol<))))

(defun point-closure (point grammar &optional (symbols ()))
  (if (null (point-position point))
      ()
      (symbol-closure (first (point-position point)) grammar symbols)))

(defun join-points (points)
  (sort (remove-duplicates points :test #'equal) #'point<))

(defun point-goto (point symbol)
  (if (eq (first (point-position point)) symbol)
      `(,(list (point-production point) (rest (point-position point)) ()))
      ()))

(defun points-closure (points grammar)
  (list (join-points (first points))
	(multi-union (append (mapcar #'(lambda (x) (point-closure x grammar)) (first points))
			     (list (second points))))))

(defun points-set (points grammar)
  (append (first points)
	  (apply #'append (mapcar #'(lambda (x) (make-points x grammar)) (second points)))))

(defun points-goto (points symbol grammar)
  (list (join-points (apply #'append 
			    (mapcar #'(lambda (x) (point-goto x symbol)) 
				    (points-set points grammar))))
	()))
      


	       