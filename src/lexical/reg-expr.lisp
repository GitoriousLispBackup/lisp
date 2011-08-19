(in-package :burning-lexical)

(defun maybe-node (child)
  "Node representing maybe expression for child"
  (or-node () child))

(defun ?repeat-node (child times)
  "Node representing repetition of child for up to 'times' times"
  (cond
    ((= 0 times) ())
    ((= 1 times) (maybe-node child))
    (t (and-node (?repeat-node child (- times 1)) (maybe-node child)))))

(defun repeat-node (child times)
  "Node representing repetition of child for 'times' times"
  (cond
    ((= 0 times) ())
    ((= 1 times) child)
    (t (and-node (repeat-node child (- times 1)) child))))

(defun full-repeat-node (child min-times max-times)
  "Node representing repetition of child for 'min-times' to 'max-times' times"
  (cond 
    ((null max-times) (repeat-node child min-times))
    ((< max-times min-times) (error "Max times in repeat lesser than min times."))
    ((= min-times max-times) (repeat-node child min-times))
    ((= 0 min-times) (?repeat-node child max-times))
    (t (and-node (repeat-node child min-times) (?repeat-node child (- max-times min-times))))))

(defun regular-to-tree (expr)
  "Converts regular expression to parsed tree"
  (cond ((null expr) nil)
        ((consp expr) (list-to-tree expr))
        (t (error "Given regular expression is not list."))))

(defun list-to-tree (expr &optional (node-generator #'and-node))
  "Converts list of atom regular expressions to parsed tree"
  (cond ((null expr) nil)
        ((null (cdr expr)) (atom-to-tree (car expr)))
        (t (funcall node-generator (atom-to-tree (car expr)) (list-to-tree (cdr expr) node-generator)))))

(defun atom-to-tree (expr)
  "Converts atom regular expression to parsed tree"
  (cond ((characterp expr) (character-node expr))
        ((stringp expr) (string-to-tree expr))
        ((consp expr) (operation-to-tree expr))
        (t (error "Wrong non-character atom."))))

(defun string-to-tree (string &optional (pos 0))
  "Converts string expression to parsed tree"
  (cond ((= pos (length string)) ())
        ((= pos (- (length string) 1)) (atom-to-tree (char string pos)))
        (t (and-node (atom-to-tree (char string pos)) (string-to-tree string (1+ pos)))))) 

(defgeneric parse-operation (operation expr))
(defmacro defoperation (name arg-name &body body)
  `(defmethod parse-operation ((operation (eql ',name)) ,@arg-name)
     ,@body))

(defoperation * (expr)
  (star-node (list-to-tree (cdr expr))))

(defoperation + (expr)
  (and-node (list-to-tree (cdr expr))
	    (star-node (list-to-tree (cdr expr)))))

(defoperation || (expr)
  (list-to-tree (cdr expr) #'or-node))

(defoperation ? (expr)
  (maybe-node (list-to-tree (cdr expr))))

(defoperation ?repeat (expr)
  (?repeat-node (list-to-tree (second expr))
		(third expr)))

(defoperation repeat (expr)
  (full-repeat-node (list-to-tree (second expr))
		    (third expr) (fourth expr)))

(defoperation - (expr)
  (range-node (character (second expr))
	      (character (third expr))))

(defun operation-to-tree (expr)
  "Converts operation to parsed tree"
  (parse-operation (car expr) expr))

(defun make-lexeme (name expr)
  "Makes lexeme with name 'name' and expresison 'expr'"
  (and-node (regular-to-tree expr) (list 'final name)))