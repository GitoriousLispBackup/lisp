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

(defstruct lexic expression follow-vector value-vector next-vector)

(defun expression (lexic)
  (slot-value lexic 'expression))

(defun follow-vector (lexic)
  (slot-value lexic 'follow-vector))

(defun make-lexic (&rest lexemes)
  (let* ((generator (integer-generator 0))
	 (lex (do-make-lexic lexemes generator))
	 (size (funcall generator)))
    (make-instance 'lexic :expression lex
    :follow-vector (make-array size :initial-element nil)
    :value-vector (make-array size :initial-element nil)
    :next-vector (make-array size :initial-element nil))))

(defmacro deflexeme (name value)
  `(defparameter ,name (make-lexeme ',name ',value)))

(defmacro deflexic (name &rest lexemes)
  `(progn
     (defparameter ,name (make-lexic ,@lexemes))
     (fill-follow-pos (expression ,name) (follow-vector ,name))
     (fill-values (expression ,name) (slot-value ,name 'value-vector) (slot-value ,name 'next-vector))))

(defun lexeme-true (lexeme) t)
(defun lexeme-false (lexeme) nil)

(defmacro defprop (node name args &body body)
  `(setf (get ',node ',name) 
	 (lambda ,args ,@body)))

(defprop nil nullable (lexeme) t)
(defprop character nullable (lexeme) nil)
(defprop final nullable (lexeme) nil)
(defprop star nullable (lexeme) t)

(defprop and nullable (lexeme)
  (and (nullable (second lexeme))
       (nullable (third lexeme))))

(defprop or nullable (lexeme)
  (or (nullable (second lexeme))
      (nullable (third lexeme))))
  
(defun lexeme-function (name &rest args)
  (let ((lexeme (car args)))
    (cond 
      ((null (car lexeme)) (apply (get nil name) args))
      ((characterp (car lexeme)) (apply (get 'character name) args))
      (t (apply (get (car lexeme) name) args)))))

(defun nullable (lexeme)
  (lexeme-function 'nullable lexeme))

(defun make-set (first second)
  (sort (copy-list (union first second)) #'<))

(defprop nil first-pos (lexeme) ())

(defprop character first-pos (lexeme) 
  (list (second lexeme)))

(defprop final first-pos (lexeme)
  (list (third lexeme)))

(defprop and first-pos (lexeme)
  (if (nullable (second lexeme))
      (make-set (first-pos (second lexeme)) (first-pos (third lexeme)))
      (first-pos (second lexeme))))

(defprop or first-pos (lexeme)
  (make-set (first-pos (second lexeme))
	    (first-pos (third lexeme))))

(defprop star first-pos (lexeme)
  (first-pos (second lexeme)))

(defun first-pos (lexeme)
  (lexeme-function 'first-pos lexeme))

(defprop nil last-pos (lexeme) ())

(defprop character last-pos (lexeme)
  (list (second lexeme)))

(defprop final last-pos (lexeme)
  (list (third lexeme)))

(defprop and last-pos (lexeme)
  (if (nullable (third lexeme))
      (make-set (last-pos (second lexeme)) (last-pos (third lexeme)))
      (last-pos (third lexeme))))

(defprop or last-pos (lexeme)
  (make-set (last-pos (second lexeme))
	    (last-pos (third lexeme))))

(defprop star last-pos (lexeme)
  (last-pos (second lexeme)))

(defun last-pos (lexeme)
  (lexeme-function 'last-pos lexeme))

(defprop nil follow-pos (lexeme vector) ())
(defprop character follow-pos (lexeme vector) ())
(defprop final follow-pos (lexeme vector) ())

(defprop or follow-pos (lexeme vector)
  (fill-follow-pos (second lexeme) vector)
  (fill-follow-pos (third lexeme) vector))

(defprop and follow-pos (lexeme vector)
  (let ((fst (first-pos (third lexeme)))
	(lst (last-pos (second lexeme))))
    (set-vector lst fst vector))
  (fill-follow-pos (second lexeme) vector)
  (fill-follow-pos (third lexeme) vector))

(defun set-vector (positions values vector)
  (dolist (position positions)
    (setf (elt vector position) 
	  (make-set (elt vector position) values))))

(defprop star follow-pos (lexeme vector)
  (let ((fst (first-pos (second lexeme)))
	(lst (last-pos (second lexeme))))
    (set-vector lst fst vector))
  (fill-follow-pos (second lexeme) vector))

(defun fill-follow-pos (lexeme vector)
  (lexeme-function 'follow-pos lexeme vector))

(defun fill-values (lexeme values next)
  (lexeme-function 'value lexeme values next))

(defprop nil value (lexeme v n) nil)

(defprop character value (lexeme v next) 
  (setf (elt next (second lexeme))
	(first lexeme)))

(defprop final value (lexeme vector n) (setf (elt vector (third lexeme))
					     (second lexeme)))

(defprop and value (lexeme value n)
  (fill-values (second lexeme) value n)
  (fill-values (third lexeme) value n))

(defprop or value (lexeme value n)
  (fill-values (second lexeme) value n)
  (fill-values (third lexeme) value n))

(defprop star value (lexeme value n)
  (fill-values (second lexeme) value n))