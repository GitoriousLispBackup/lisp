(in-package :burning-lexical)

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

(defclass lexic () 
  ((expression :initarg :expression :accessor expression)
   (follow-vector :initarg :follow-vector :accessor follow-vector)
   (value-vector :initarg :value-vector :accessor value-vector)
   (next-vector :initarg :next-vector :accessor next-vector)))

(defun make-lexic (&rest lexemes)
  (make-instance 'lexic :lexemes lexemes))

(defmethod initialize-instance :after ((lexic lexic) &key lexemes)
  (let* ((generator (integer-generator 0))
	 (lex (do-make-lexic lexemes generator))
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

(defclass state-machine () 
  ((values :initarg :values)
   (transitions :initarg :transitions)))

(defun make-state-machine ()
  (make-instance 'state-machine 
		 :values (make-array 0 :adjustable t :fill-pointer 0)
		 :transitions (make-array 0 :adjustable t :fill-pointer 0)))

(defun machine-values (machine)
  (slot-value machine 'values))

(defun machine-transitions (machine)
  (slot-value machine 'transitions))

(defun fill-nexts (state lexic table)
  (let ((next (elt (next-vector lexic) state)))
    (unless (null next)
      (setf (gethash next table)
	    (make-set (copy-list (elt (follow-vector lexic) state))
		      (gethash next table))))))

(defun add-machine-state (machine)
  (vector-push-extend nil (machine-values machine))
  (vector-push-extend nil (machine-transitions machine))
  (- (length (machine-values machine)) 1))

(defun add-state-function (state machine visited-states &aux (new-states nil))
  (list 
   (lambda (next next-state)
     (let ((next-state-id (gethash next-state visited-states)))
       (unless next-state-id
	 (setq next-state-id (add-machine-state machine))
	 (push next-state new-states)
	 (setf (gethash next-state visited-states) next-state-id))
       (push (cons next next-state-id) 
	     (elt (machine-transitions machine) 
		  (gethash state visited-states)))))
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
  (setf (elt (machine-values machine) (gethash state visited-states))
	(state-value state lexic))
  (setq next-states (set-state-transitions state lexic machine visited-states))
  (dolist (next-state next-states)
    (process-state next-state lexic machine visited-states)))

(defun create-state-machine (lexic)
  (let ((machine (make-state-machine))
	(visited-states (make-hash-table :test 'equal))
	(first-state (first-pos (expression lexic))))
    (setf (gethash first-state visited-states) 0)
    (add-machine-state machine)
    (process-state first-state lexic machine visited-states)
    machine))

(defun machine-next (machine state char)
  (cdr (assoc char (elt (machine-transitions machine)
			state))))

(defun machine-value (machine string &optional (state 0) (pos 0))
  (cond 
    ((null state) nil)
     ((= (length string) pos) (elt (machine-values machine) state))
     (t (machine-value machine string (machine-next machine state (char string pos))
		                     (1+ pos)))))
      