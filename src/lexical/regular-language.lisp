(in-package :burning-lexical)

;;
;;Range sets
;;

(defun range< (range1 range2)
  (and (char< (first range1) (first range2))
       (char< (rest range1) (first range2))))

(defun range<= (range1 range2)
  (and (char< (first range1) (first range2))
       (char< (rest range1) (code-char (1- (char-code (first range2)))))))

(defun char-min (char1 char2)
  (code-char (min (char-code char1) (char-code char2))))

(defun char-max (char1 char2)
  (code-char (max (char-code char1) (char-code char2))))

(defun merge-ranges (range1 range2)
  (cons (char-min (first range1) (first range2))
	(char-max (rest range1) (rest range2))))

(defun sub-ranges (range1 range2)
  (let ((first (char-max (first range1) (first range2)))
	(last (char-min (rest range1) (rest range2))))
    (let ((first-range (if (char> first (first range1))
			   (list (cons (first range1) (code-char (1- (char-code first)))))
			   ()))
	  (last-range (if (char< last (rest range1))
			  (list (cons (code-char (1+ (char-code last))) (rest range1)))
			  ())))
      (append first-range last-range))))

(defun merge-sets (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	((range<= (first set1) (first set2)) (cons (first set1) (merge-sets (rest set1) set2)))
	((range<= (first set2) (first set1)) (merge-sets set2 set1))
	(t (merge-sets (list (merge-ranges (first set1) (first set2)))
		       (merge-sets (rest set1) (rest set2))))))
	
(defun sub-sets (set1 set2)
  (cond ((null set1) nil)
	((null set2) set1)
	((range< (first set1) (first set2)) (cons (first set1) (sub-sets (rest set1) set2)))
	((range< (first set2) (first set1)) (sub-sets set1 (rest set2)))
	(t (sub-sets (append (sub-ranges (first set1) (first set2))
			     (rest set1))
		     set2))))

(defgeneric to-range-set (node))
(defmethod to-range-set (node)
  (error "Expressions containing ~a cannot be converted to range set." (type-of node)))

(defmethod to-range-set ((node range-node))
  (list (cons (range-first node) (range-last node))))

(defmethod to-range-set ((node or-node))
  (merge-sets (to-range-set (left-node node))
	      (to-range-set (right-node node))))

(defmethod to-range-set ((node empty-node))
  ())

(defun range-to-node (range)
  (range-node (first range) (rest range)))

(defun range-set-to-node (set)
  (cond 
    ((null set) (empty-node))
    ((null (rest set)) (range-to-node (first set)))
    (t (or-node (range-to-node (first set)) (range-set-to-node (rest set))))))
;;
;;Operations
;;

(defun :char (char) 
  (character-node char))

(defun :range (first last)
  (when (char> first last)
    (error "First character in range is ~a, that is greater then ~a." first last))
  (range-node first last))

(defun :or (left right)
  (or-node left right))

(defun :empty ()
  (empty-node))

(defun :not (node &key (full-range (any-character-node)))
  (range-set-to-node
   (sub-sets (to-range-set full-range) 
	     (to-range-set node))))

(defun :all ()
  (any-character-node))

(defun :any ()
  (:not (:char #\Newline)))

(defgeneric upper-letters (language))
(defgeneric lower-letters (language))

(defmethod upper-letters (language)
  (error "Sorry, language ~a not supported yet. Your help in this field would be usefull." language))

(defmethod lower-letters (language)
  (error "Sorry, language ~a not supported yet. Your help in this field would be usefull." language))

(defun letters (language no-upper no-lower)
  (cond (no-upper (lower-letters language))
	(no-lower (upper-letters language))
	(t (:or (lower-letters language) (upper-letters language)))))

(defun :letter (&key (no-upper nil) (no-lower nil) (languages '(en)))
  (cond ((null languages) (empty-node))
	((null (rest languages)) (letters (first languages) no-upper no-lower))
	(t (:or (letters (first languages) no-upper no-lower) 
		(:letter :no-upper no-upper
			 :no-lower no-lower
			 :languages (rest languages))))))

(defmethod upper-letters ((language (eql 'en)))
  (:range #\A #\Z))

(defmethod lower-letters ((language (eql 'en)))
  (:range #\a #\z))

(defmethod upper-letters ((language (eql 'ru)))
  (:range #\А #\Я))

(defmethod lower-letters ((language (eql 'ru)))
  (:range #\а #\я))

(defun :digit ()
  (:range #\0 #\9))

(defun :hex-digit ()
  (:or (:range #\0 #\9)
       (:or (:range #\a #\f)
	    (:range #\A #\F))))

(defun :blank ()
  (:or (:char #\Space) (:char #\Tab)))

(defun :space ()
  (:or (:char #\Space)
       (:or (:char #\Tab) (:char #\Newline))))

(defun :and (left right)
  (and-node left right))

(defun :star (expr)
  (star-node expr))

(defun :positive (expr)
  (:and expr (:star expr)))

(defun repeat (expr times)
  (cond ((> times 1) (:and expr (repeat expr (1- times))))
	((= times 1) expr)
	(t (error "Wrong argument ~a for repeat" times))))

(defun ?repeat (expr times)
  (repeat (:or (:empty) expr) times))

(defun :repeat (expr min-times max-times)
  (cond ((and (= min-times 0) (= max-times 0)) (:empty))
	((= min-times 0) (?repeat expr max-times))
	((= max-times min-times) (repeat expr min-times))
	((< max-times min-times) (error "~a is less than ~a in repeat" max-times min-times))
	(t (:and (repeat expr min-times) (?repeat expr (- max-times min-times))))))

(defun :maybe (expr)
  (:or (:empty) expr))

(defclass lexeme ()
  ((name :initarg :name :reader lexeme-name)
   (expression :initarg :expression :reader lexeme-expression)
   (minimalp :initarg :minimalp :initform nil :reader lexeme-minimal-p)
   (skippedp :initarg :skippedp :initform nil :reader lexeme-skipped-p)))

(defmethod lexeme-name ((object (eql nil)))
  nil)

(defmethod lexeme-minimal-p ((object (eql nil)))
  nil)

(defmethod oequal ((l1 lexeme) (l2 lexeme))
  (and (eq (lexeme-name l1) (lexeme-name l2))
       (eq (lexeme-expression l1) (lexeme-expression l2))
       (eq (lexeme-minimal-p l1) (lexeme-minimal-p l2))
       (eq (lexeme-skipped-p l1) (lexeme-skipped-p l2))))

(defmethod print-object ((object lexeme) stream)
  (format stream "#s(lexeme :name ~a :expression ~a :minimalp ~a :skipped ~a)" 
	  (lexeme-name object) 
	  (lexeme-expression object)
	  (lexeme-minimal-p object)
	  (lexeme-skipped-p object)))

(defgeneric lexeme-to-node (object))
(defmethod lexeme-to-node ((lexeme lexeme))
  (:and (lexeme-expression lexeme) (final-node lexeme)))

(defun make-lexeme (name expression &key (minimal nil) (skipped nil))
  (make-instance 'lexeme 
		 :expression expression
		 :name name  
		 :minimalp minimal 
		 :skippedp skipped))

(defmacro deflexeme (name expression &key (minimal nil) (skipped nil))
  `(defparameter ,name (make-lexeme ',name ,expression 
				    ,@(when minimal '(:minimal t)) ,@(when skipped '(:skipped t)))))
