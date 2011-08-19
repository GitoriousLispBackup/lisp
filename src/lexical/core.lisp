(in-package :burning-lexical)

(defun empty-node ()
  ())

(defun character-node (char)
  "Node containing one character"
  (list char char))

(defun range-node (first last)
  "Node representing character range from 'first' till 'last'"
  (list first last))

(defun and-node (left right)
  "Node representing and expression for left and right"
  (list 'and left right))

(defun star-node (child)
  "Node representing star expression for child"
  (list 'star child))

(defun or-node (left right)
  "Node representing or expression for left and right"
  (list 'or left right))

(defun integer-generator (n)
  (decf n)
  (lambda () (incf n)))

(defun add-positions (lexeme generator)
  (cond ((null lexeme) lexeme)
	((integerp (first lexeme)) (append lexeme (list (funcall generator))))
	((eq (first lexeme) 'final) (append lexeme (list (funcall generator))))
	(t (cons (first lexeme) (mapcar #'(lambda (x) (add-positions x generator)) (rest lexeme))))))

(defun merge-lexemes (lexemes)
  (cond ((null lexemes) ())
	((null (rest lexemes)) (first lexemes))
	(t (or-node (first lexemes) (merge-lexemes (rest lexemes))))))

(defun do-make-lexic (lexemes generator)
  (add-positions (merge-lexemes lexemes) generator))

(defclass lexic () 
  ((expression :initarg :expression :accessor expression)
   (translation :initarg :translation :accessor translation)
   (follow-vector :initarg :follow-vector :accessor follow-vector)
   (value-vector :initarg :value-vector :accessor value-vector)
   (next-vector :initarg :next-vector :accessor next-vector)))

(defun make-lexic (&rest lexemes)
  (make-instance 'lexic :lexemes lexemes))

(defun do-split (list char)
  (let ((head (first list)))
    (cond
      ((< (rest head) char) (do-split (rest list) char))
      ((= (first head) char) t)
      (t 
       (let ((new-node (list (cons char (rest head)))))
	 (setf (rest new-node) (rest list))
	 (setf (rest list) new-node)
	 (setf (rest (first list)) (- char 1)))))))

(defun split (translation first last)
  (do-split translation (char-code first))
  (do-split translation (1+ (char-code last))))

(defun do-make-translation (expr translation)
  (cond 
    ((null expr) t)
    ((characterp (first expr)) (split translation (first expr) 
				                  (second expr)))
    ((eq (first expr) 'final) t)
    (t (mapc #'(lambda (x) (do-make-translation x translation)) (rest expr)))))
      
(defun translation-apply-fun ()
  (let ((generator (integer-generator 0)))
    (lambda (range)
      (list (cons (code-char (first range))
		  (code-char (rest range)))
	    (funcall generator)))))

(defun make-translation (lexic)
  (let ((translation (list (cons -1 (1+ char-code-limit)))))
    (do-make-translation (expression lexic) translation)
    (setq translation (butlast (rest translation)))
    (setq translation (mapcar (translation-apply-fun) translation))
    (setf (translation lexic) translation)))

(defun translate (char translation)
  (if (null translation)
      nil
      (let ((range (first (first translation)))
	    (value (second (first translation))))
	(cond
	  ((char< char (first range)) nil)
	  ((char< (rest range) char) (translate char (rest translation)))
	  (t value)))))

(defun split-ranges (expr translation)
  (cond 
    ((null expr) nil)
    ((characterp (first expr)) (range-to-tree (translate (first expr) translation)
					      (translate (second expr) translation)))
    ((eq (first expr) 'final) expr)
    (t (cons (first expr) (mapcar #'(lambda (x) (split-ranges x translation))
				  (rest expr))))))
  
(defun range-to-tree (first last)
  (cond ((= first last) (list first))
	(t (or-node (list first) (range-to-tree (1+ first) last)))))

(defmethod initialize-instance :after ((lexic lexic) &key lexemes)
  (setf (expression lexic) (merge-lexemes lexemes))
  (setf (translation lexic) (make-translation lexic))
  (setf (expression lexic) (split-ranges (expression lexic)
					 (translation lexic)))
  (let* ((generator (integer-generator 0))
	 (lex (add-positions (expression lexic) generator))
	 (size (funcall generator)))
    (setf (expression lexic) lex)
    (setf (follow-vector lexic) (make-array size :initial-element nil))
    (setf (value-vector lexic) (make-array size :initial-element nil))
    (setf (next-vector lexic) (make-array size :initial-element nil))))

(defmacro deflexeme (name value)
  `(defparameter ,name (make-lexeme ',name ',value)))

(defmacro deflexic (name &rest lexemes)
  `(progn
     (defparameter ,name (make-lexic ,@lexemes))
     (fill-follow-pos (expression ,name) (follow-vector ,name))
     (fill-values (expression ,name) (slot-value ,name 'value-vector) (slot-value ,name 'next-vector))))

(defmacro defprop (node name args &body body)
  `(setf (get ',node ',name) 
	 (lambda ,args ,@body)))

(defprop nil nullable (lexeme)
  (declare (ignore lexeme))
  t)

(defprop character nullable (lexeme)
  (declare (ignore lexeme))
  nil)

(defprop final nullable (lexeme)
  (declare (ignore lexeme))
  nil)

(defprop star nullable (lexeme)
  (declare (ignore lexeme))
  t)

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
      ((integerp (car lexeme)) (apply (get 'character name) args))
      (t (apply (get (car lexeme) name) args)))))

(defun nullable (lexeme)
  (lexeme-function 'nullable lexeme))

(defun make-set (first second)
  (sort (copy-list (union first second)) #'<))

(defprop nil first-pos (lexeme) 
  (declare (ignore lexeme))
  ())

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

(defprop nil last-pos (lexeme) 
  (declare (ignore lexeme))
  ())

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

(defprop nil follow-pos (lexeme vector) 
  (declare (ignore lexeme vector))
  ())

(defprop character follow-pos (lexeme vector) 
  (declare (ignore lexeme vector))
  ())

(defprop final follow-pos (lexeme vector)
  (declare (ignore lexeme vector))
  ())

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

(defprop nil value (lexeme v n) 
  (declare (ignore lexeme v n))
  nil)

(defprop character value (lexeme v next) 
  (declare (ignore v))
  (setf (elt next (second lexeme))
	(first lexeme)))

(defprop final value (lexeme vector n)
  (declare (ignore n))
  (setf (elt vector (third lexeme))
	(second lexeme)))

(defprop and value (lexeme value n)
  (fill-values (second lexeme) value n)
  (fill-values (third lexeme) value n))

(defprop or value (lexeme value n)
  (fill-values (second lexeme) value n)
  (fill-values (third lexeme) value n))

(defprop star value (lexeme value n)
  (fill-values (second lexeme) value n))


