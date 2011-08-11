(in-package :burning-lexical)

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

(defclass state-machine () 
  ((translation :initarg :translation)
   (values :initarg :values 
	   :initform (make-array 0 :adjustable t :fill-pointer 0))
   (transitions :initarg :transitions 
		:initform (make-array 0 :adjustable t :fill-pointer 0))))

(defgeneric (setf translation) (value machine))
(defgeneric add-state (machine))
(defgeneric value (machine state))
(defgeneric (setf value) (value machine state))
(defgeneric next (machine state code))
(defgeneric (setf next) (value machine state code))
(defgeneric first-state (machine))

(defmethod first-state ((machine state-machine))
  0)

(defmethod fill-translation (translation first last value)
  (do ((i first (1+ i)))
      ((> i last) t)
    (setf (elt translation i) value)))

(defmethod (setf translation) (value (machine state-machine))    
  (setf (slot-value machine 'translation)
	(make-array (1+ (char-code (cdaar (last value)))) :initial-element -1))
  (dolist (range value)
    (fill-translation (slot-value machine 'translation)
		      (char-code (caar range))
		      (char-code (cdar range))
		      (second range))))

(defmethod add-state ((machine state-machine))
  (with-slots (values transitions) machine
    (vector-push-extend nil values)
    (vector-push-extend nil transitions)
    (- (length values) 1)))

(defmethod value ((machine state-machine) state)
  (elt (slot-value machine 'values) state))

(defmethod (setf value) (value (machine state-machine) state)
  (setf (elt (slot-value machine 'values) state)
	value))

(defmethod next ((machine state-machine) state code)
  (if (null code)
      nil
      (cdr (assoc (elt (slot-value machine 'translation) (char-code code))
		  (elt (slot-value machine 'transitions) 
		       state)))))

(defmethod (setf next) (value (machine state-machine) state code)
  (push (cons code value)
	(elt (slot-value machine 'transitions) state)))

(defun fill-nexts (state lexic table)
  (let ((next (elt (next-vector lexic) state)))
    (unless (null next)
      (setf (gethash next table)
	    (make-set (copy-list (elt (follow-vector lexic) state))
		      (gethash next table))))))

(defun add-state-function (state machine visited-states &aux (new-states nil))
  (list 
   (lambda (next next-state)
     (let ((next-state-id (gethash next-state visited-states)))
       (unless next-state-id
	 (setq next-state-id (add-state machine))
	 (push next-state new-states)
	 (setf (gethash next-state visited-states) next-state-id))
       (setf (next machine 
		   (gethash state visited-states)
		   next) 
	     next-state-id)))
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
  (setf (value machine (gethash state visited-states))
	(state-value state lexic))
  (setq next-states (set-state-transitions state lexic machine visited-states))
  (dolist (next-state next-states)
    (process-state next-state lexic machine visited-states)))

(defun create-state-machine (lexic &optional (machine-class 'state-machine))
  (let ((machine (make-instance machine-class))
	(visited-states (make-hash-table :test 'equal))
	(first-state (first-pos (expression lexic))))
    (setf (gethash first-state visited-states) (add-state machine))
    (process-state first-state lexic machine visited-states)
    (setf (translation machine) (translation lexic))
    machine))

(defun machine-value (machine string &optional (state 0) (pos 0))
  (cond 
    ((null state) nil)
     ((= (length string) pos) (value machine state))
     (t (machine-value machine string (next machine state (char string pos))
		       (1+ pos)))))

(defun get-token (iterator machine)
  (do ((result "")
       (type nil)
       (state (first-state machine)))
      ((null state) (cond ((string= result "") (error "Wrong token ~a" (commit iterator)))
			  (t (reset iterator) (cons result type))))
    (when (value machine state)
      (setq type (value machine state))
      (setq result (concatenate 'string result (commit iterator))))
    (setq state (next machine state (get-next iterator)))))

(defun print-stream (iterator machine)
  (do ()
      ((eof-p iterator) t)
    (let ((token (get-token iterator machine)))
      ())))
    
  