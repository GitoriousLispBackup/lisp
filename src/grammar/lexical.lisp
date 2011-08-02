(defun character-node (char)
  (list char))

(defun and-node (left right)
  (list 'and left right))

(defun star-node (child)
  (list 'star (list-to-tree child)))

(defun or-node (left right)
  (list 'or left right))

(defun maybe-node (child)
  (or-node () child))

(defun ?repeat-node (child times)
  (cond
    ((= 0 times) ())
    ((= 1 times) (maybe-node child))
    (t (and-node (?repeat-node child (- times 1)) (maybe-node child)))))

(defun repeat-node (child times)
  (cond
    ((= 0 times) ())
    ((= 1 times) child)
    (t (and-node (repeat-node child (- times 1)) child))))

(defun full-repeat-node (child min-times max-times)
  (cond 
    ((null max-times) (repeat-node child min-times))
    ((< max-times min-times) (error "Max times in repeat lesser than min times."))
    ((= min-times max-times) (repeat-node child min-times))
    ((= 0 min-times) (?repeat-node child max-times))
    (t (and-node (repeat-node child min-times) (?repeat-node child (- max-times min-times))))))

(defun range-node (first last)
  (let ((value1 (char-code first)) (value2 (char-code last)))
     (cond ((> value1 value2) (error "First symbol in range greater than last."))
	   ((= value1 value2) (character-node first))
           (t (or-node (character-node first) (range-node (character (1+ value1)) last))))))

(defun regular-to-tree (expr)
  (cond ((null expr) nil)
        ((consp expr) (list-to-tree expr))
        (t (error "Given regular expression is not list."))))

(defun list-to-tree (expr &optional (node-generator #'and-node))
  (cond ((null expr) nil)
        ((null (cdr expr)) (atom-to-tree (car expr)))
        (t (funcall node-generator (atom-to-tree (car expr)) (list-to-tree (cdr expr) node-generator)))))

(defun atom-to-tree (expr)
  (cond ((characterp expr) (character-node expr))
        ((stringp expr) (string-to-tree expr))
        ((consp expr) (operation-to-tree expr))
        (t (error "Wrong non-character atom."))))

(defun string-to-tree (string &optional (pos 0))
  (cond ((= pos (length string)) ())
        ((= pos (- (length string) 1)) (atom-to-tree (char string pos)))
        (t (and-node (atom-to-tree (char string pos)) (string-to-tree string (1+ pos)))))) 

(defun operation-to-tree (expr)
  (cond ((eq (car expr) '*) (star-node (cdr expr)))
        ((eq (car expr) '+) (and-node (list-to-tree (cdr expr)) (star-node (cdr expr))))
        ((eq (car expr) '||) (list-to-tree (cdr expr) #'or-node))
        ((eq (car expr) '?) (maybe-node (list-to-tree (cdr expr))))
        ((eq (car expr) '?repeat) (?repeat-node (list-to-tree (second expr)) (third expr)))
        ((eq (car expr) 'repeat) (full-repeat-node (list-to-tree (second expr)) (third expr) (fourth expr)))
        ((eq (car expr) '-) (range-node (character (second expr)) (character (third expr))))
        (t (error "Wrong operation"))))

(defun make-lexeme (name expr)
  (and-node (regular-to-tree expr) (list 'final name)))

(defun integer-generator (n)
  (decf n)
  (lambda () (incf n)))

(defun add-positions (lexeme generator)
  (cond ((null lexeme) lexeme)
	((characterp (first lexeme)) (append lexeme (list (funcall generator))))
	((eq (first lexeme) 'final) (append lexeme (list (funcall generator))))
	(t (cons (first lexeme) (mapcar #'(lambda (x) (add-positions x generator)) (rest lexeme))))))

(defun do-make-lexic (lexemes generator)
  (cond ((null lexemes) ())
	((null (rest lexemes)) (add-positions (car lexemes) generator))
	(t (or-node (add-positions (car lexemes) generator) (do-make-lexic (rest lexemes) generator)))))

(defstruct lexic expression follow-vector)

(defun make-lexic (&rest lexemes)
  (let ((generator (integer-generator 0)))
    (make-instance 'lexic :expression (do-make-lexic lexemes generator) 
		   :follow-vector (make-array (funcall generator)))))

(defmacro deflexeme (name value)
  `(defparameter ,name (make-lexeme ',name ',value)))

(defun nullable (lexeme)
  (cond ((null (car lexeme)) t)
	((characterp (car lexeme)) nil)
        ((eq 'finish (car lexeme)) nil)
	((eq 'and (car lexeme)) (and (nullable (second lexeme))
				     (nullable (third lexeme))))
	((eq 'or (car lexeme)) (or (nullable (second lexeme))
				   (nullable (third lexeme))))
	((eq 'star (car lexeme)) t)))

(defun first-pos (lexeme)
  (cond ((null (car lexeme)) ())
	((characterp (car lexeme)) (list (second lexeme)))
	((eq (car lexeme) 'final) (list (third lexeme)))
	((eq (car lexeme) 'and) (first-pos (second lexeme)))
	((eq (car lexeme) 'or) (sort (union (first-pos (second lexeme)) 
					    (first-pos (third lexeme)))
				     #'<))))
