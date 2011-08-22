(in-package :burning-lexical)

;;Base node class

(defclass node () ())

(defgeneric node-childs (node))
(defmethod node-childs ((node node))
  ())

(defgeneric (setf node-childs) (value node))
(defmethod (setf node-childs) (value node)
  (when (> (length value) 0)
      (error "Node of type ~a has no childs" (type-of node))))

(defclass positioned-node (node)
  ((position :initarg position :accessor node-position)))

;;Empty node

(defclass empty-node (node) ())

(defun empty-node ()
  (make-instance 'empty-node))

;;Final node

(defclass final-node (positioned-node) 
  ((lexeme :initarg :lexeme :accessor node-lexeme)))

(defun final-node (lexeme)
  (make-instance 'final-node :lexeme lexeme))

;;Range node

(defclass range-node (node)
  ((first :initarg :first :reader range-first)
   (last :initarg :last :reader range-last)))

(defun character-node (char)
  "Node containing one character"
  (make-instance 'range-node :first char :last char))

(defun range-node (first last)
  "Node representing character range from 'first' till 'last'"
  (make-instance 'range-node :first first :last last))

;;Integer range node

(defclass integer-range-node (positioned-node)
  ((range :initarg :range :reader range-value)))

(defun integer-range-node (value)
  (make-instance 'integer-range-node :range value))

;;Star node

(defclass star-node (node)
  ((child :initarg :child)))

(defun star-node (child)
  "Node representing star expression for child"
  (make-instance 'star-node :child child))

(defmethod node-childs ((node star-node))
  (list (slot-value node 'child)))

(defmethod (setf node-childs) (value (node star-node))
  (if (/= (length value) 1)
      (error "Star node has only one child.")
      (setf (slot-value node 'child)
	    (first value))))

;;Binary node

(defclass binary-node (node)
  ((left :initarg :left)
   (right :initarg :right)))

(defmethod node-childs ((node binary-node))
  (list (slot-value node 'left)
	(slot-value node 'right)))

(defmethod (setf node-childs) (value (node binary-node))
  (if (/= (length value) 2)
      (error "Binary node has 2 childs.")
      (progn
	(setf (slot-value node 'left) (first value))
	(setf (slot-value node 'right) (second value)))))

;;And node

(defclass and-node (binary-node)())

(defun and-node (left right)
  "Node representing and expression for left and right"
  (make-instance 'and-node :left left :right right))

;;Or node

(defclass or-node (binary-node)())

(defun or-node (left right)
  "Node representing or expression for left and right"
  (make-instance 'or-node :left left :right right))

;;Etc

(defun integer-generator (n)
  (decf n)
  (lambda () (incf n)))

(defgeneric add-positions (lexeme number-generator))
(defmethod add-positions ((lexeme node) generator)
  (mapc #'(lambda (x) (add-positions x generator)) (node-childs lexeme)))

(defmethod add-positions :after ((lexeme positioned-node) generator)
  (setf (node-position lexeme) (funcall generator)))

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

(defgeneric do-make-translation (node translation))
(defmethod do-make-translation ((node node) translation)
  (mapc #'(lambda (x) (do-make-translation x translation)) (node-childs node)))

(defmethod do-make-translation ((node range-node) translation)
  (split translation (range-first node) (range-last node)))

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

(defgeneric split-ranges (expr translation))
(defmethod split-ranges (expr translation)
  (setf (node-childs expr)
	(mapcar #'(lambda (x) (split-ranges x translation)) (node-childs expr)))
  expr)

(defmethod split-ranges ((expr range-node) translation)
  (range-to-tree (translate (range-first expr) translation)
		 (translate (range-last expr) translation)))

(defun range-to-tree (first last)
  (cond ((= first last) (integer-range-node first))
	(t (or-node (integer-range-node first) (range-to-tree (1+ first) last)))))

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


