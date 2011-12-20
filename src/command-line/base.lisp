(in-package #:burning-command-line)

;;
;; Argument base class
;;

(defclass argument ()
  ((name :initarg :name :reader argument-name)
   (short-name :initarg :short-name :reader argument-short-name)
   (description :initarg :description :reader argument-description)))

(defgeneric make-argument (class name &key short-name description &allow-other-keys))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric parse-argument-spec (class spec))
  (defmethod parse-argument-spec (class spec)
    `(make-argument ,class ,@spec)))

;;
;; Flag class 
;;

(defclass flag (argument) ())

(defmethod make-argument ((class (eql :flag)) name &key short-name description)
  (make-instance 'flag 
		 :name name
		 :short-name short-name
		 :description description))

;;
;; Key class
;;

(defclass key (argument) 
  ((type :initarg :type :reader key-argument-type)))

(defmethod make-argument ((class (eql :key)) name &key type short-name description)
  (make-instance 'key
		 :name name
		 :type type
		 :short-name short-name
		 :description description))

;;
;; Arguments list
;;

(defclass arguments-list () 
  ((arguments :initarg :arguments :accessor arguments-list-arguments)
   (parent :initform nil :reader arguments-list-parent)))

(defgeneric arguments-list-name (list))

(defun check-arguments-list (args)
  (labels ((have-same-names (args)
	     (cond
	       ((null args) nil)
	       ((have-same-name (first args) (rest args)))
	       (t (have-same-names (rest args))))))
    (let ((duplicate (have-same-names args)))
      (if duplicate (error duplicate) t))))

(defmethod initialize-instance :before ((instance arguments-list) &key arguments)
  (check-arguments-list arguments))

(defun add-argument (arg list)
  (when (have-argument-p (argument-name arg) list)
    (error 'argument-already-exists-error :name (argument-name arg)))
  (push arg (arguments-list-arguments list)))

(defun argument (name list)
  (find name (arguments-list-arguments list) :test #'equal :key #'argument-name))

(defun have-argument-p (name list)
  (or (some  #'(lambda (action) (have-argument-p name action)) (actions list))
      (if (argument name list) t nil)))

(defun string+ (&rest strings)
  (apply #'concatenate 'string strings))

(defgeneric arguments-list-description (list))

(defun help-message (list)
  (labels ((usage-message (list)
	     (string+ (if (string/= (arguments-list-description list) "")
			  (format nil "~a~%~%" (arguments-list-description list)) "")
		      (format nil "Usage:~%  ~a [ARGS]~%~%" (arguments-list-name list))))
	   (argument-name-message (arg)
	     (string+ (format nil "    --~a" (argument-name arg))
		      (if (argument-short-name arg) (format nil ",-~a" (argument-short-name arg)) "")
		      (if (typep arg 'key) (format nil " ~a" (type-value-name (key-argument-type arg))) "")))
	   (argument-description-message (arg)
	     (if (argument-description arg) (format nil "~a" (argument-description arg)) ""))
	   (arguments-message (list)
	     (let ((names (mapcar #'argument-name-message (arguments-list-arguments list)))
		   (descs (mapcar #'argument-description-message (arguments-list-arguments list))))
	       (let ((space (+ 10 (apply #'max (mapcar #'length names)))))
		 (string+ (format nil "  Where ARGS are:~%")
			  (apply #'string+ 
				 (mapcar #'(lambda (name desc) 
					     (format nil "~a~va~a~%" name (- space (length name)) "" desc))
					 names descs))
			  (format nil "~%"))))))
    (string+ (usage-message list)
	     (arguments-message list))))

(defmethod initialize-instance :after ((obj arguments-list) &key (help nil help-set-p) arguments)
  (let ((help (if help-set-p help (make-argument :flag "help" :description "Products this help message"))))
    (let ((arguments (if help (cons help arguments) arguments)))
      (setf (arguments-list-arguments obj) arguments)))
  (mapc #'(lambda (action) (setf (slot-value action 'parent) obj)) (actions obj)))

(defmacro make-arguments-list (class name-and-options argument-specs)
  (labels ((parse-option (spec)
	     (let ((name (if (listp spec) (first spec) spec)))
	       (case name
		 (:help `(:help (make-argument :flag ,@(rest spec))))
		 (:no-help `(:help nil))
		 (otherwise (list spec)))))
	   (parse-options (opts)
	     (if (stringp opts)
		 (list :name opts)
		 `(:name ,(first opts) ,@(reduce #'append (mapcar #'parse-option (rest opts)))))))
    `(make-instance ,class
		    ,@(parse-options name-and-options)
		    :arguments ,(parse-arguments-option argument-specs))))

;;
;; Action class
;;

(defclass action (argument arguments-list) ())

(defmethod make-argument ((class (eql :action)) name &key short-name description arguments)
  (make-instance 'action 
		 :name name
		 :short-name short-name
		 :description description
		 :arguments arguments))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-arguments-option (args)
    `(list ,@(mapcar #'(lambda (spec) (parse-argument-spec (first spec) (rest spec))) args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod parse-argument-spec ((class (eql :action)) spec)
    (labels ((find-key (name list)
	       (cond
		 ((null list) nil)
		 ((eq (first list) name) (second list))
		 (t (find-key name (rest list)))))
	     (remove-key (name list)
	       (cond
		 ((null list) nil)
		 ((eq (first list) name) (remove-key name (rest (rest list))))
		 (t (cons (first list) (remove-key name (rest list)))))))
      (let ((args (find-key :arguments spec))
	    (spec (remove-key :arguments spec)))
	`(make-arguments-list 'action ,spec ,args)))))

(defmethod arguments-list-description ((list action))
  (argument-description list))

(defmethod arguments-list-name ((list action))
  (string+ (if (arguments-list-parent list) 
	       (format nil "~a " (arguments-list-name (arguments-list-parent list))) "")
   (format nil "--~a" (argument-name list))
	   (if (argument-short-name list) (format nil ",-~a" (argument-short-name list)) "")))

(defun actions (arg-list)
  (remove-if-not #'(lambda (arg) (typep arg 'action)) (arguments-list-arguments arg-list)))

;;
;; Arguments specification
;;

(defclass arguments-spec (arguments-list) 
  ((name :initarg :name :reader arguments-list-name)
   (description :initarg :description :initform "" :reader arguments-list-description)))

(define-condition argument-already-exists-error (error)
  ((name :initarg :name :reader argument-already-exists-error-name)))

(define-condition short-name-already-exists-error (error)
  ((char :initarg :char :reader short-name-already-exists-error-char)))

(defun have-same-name (arg args)
  (flet ((find-name (arg args)
	   (find (argument-name arg) args :test #'equal :key #'argument-name))
	 (find-short-name (arg args)
	   (if (argument-short-name arg)
	       (find (argument-short-name arg) args :key #'argument-short-name))))
    (cond 
      ((null args) nil)
      ((find-name arg args) 
       (make-condition 'argument-already-exists-error :name (argument-name arg)))
      ((find-short-name arg args) 
       (make-condition 'short-name-already-exists-error :char (argument-short-name arg))))))

(defmacro make-arguments-spec (&optional (name-and-options "") &body argument-specs)
  `(make-arguments-list 'arguments-spec ,name-and-options ,argument-specs))

;; Parsing

(define-condition wrong-argument-error (error)
  ((string :initarg :string :reader wrong-argument-error-string)))

(define-condition wrong-short-argument-error (error)
  ((char :initarg :char :reader wrong-short-argument-error-char)))

(defun arguments-to-list (args)
  (flet ((argument-to-list (arg)
	   (assert (stringp arg))
	   (cond 
	     ((and (>= (length arg) 2) (equal (subseq arg 0 2) "--")) `((:arg ,(subseq arg 2))))
	     ((and (>= (length arg) 1) (equal (char arg 0) #\-))
	      (map 'list #'(lambda (char) `(:short ,char)) (subseq arg 1)))
	     (t `((:param ,arg))))))
    (apply #'append (mapcar #'argument-to-list args))))

(defstruct list-iterator
  list)

(defun iterator-current (iter)
  (first (list-iterator-list iter)))

(defun iterator-next (iter)
  (setf (list-iterator-list iter) (rest (list-iterator-list iter))))

(defgeneric parse-argument (arg rest-args))

(defmethod parse-argument (arg rest)
  nil)

(define-condition missed-key-value-error (error)
  ((type :initarg :type :reader missed-key-value-error-type)))

(defmethod parse-argument ((arg key) rest)
  (let ((type-spec (key-argument-type arg)))
    (destructuring-bind (type type-args) (if (listp type-spec) 
					     (list (first type-spec) (rest type-spec))
					     (list type-spec nil))
      (apply #'parse-type rest type type-args))))

(defun find-argument (name spec key)
  (labels ((find-argument-in-actions (actions)
	     (cond
	       ((null actions) nil)
	       ((find-argument name (first actions) key))
	       (t (find-argument-in-actions (rest actions))))))
    (or (find-argument-in-actions (actions spec))
	(find name (arguments-list-arguments spec) :key key :test #'equal))))

(defun %parse-argument (name key rest spec env)
  (labels ((parse-in-actions (actions env)
	     (cond 
	       ((null actions) nil)
	       ((and (argument-set-p (argument-name (first actions)) env)
		     (find-argument name (first actions) key))
		(setf (argument-value (argument-name (first actions)) env) 
		      (%parse-argument name key rest (first actions) 
				       (argument-value (argument-name (first actions)) env)))
		env)
	       (t (parse-in-actions (rest actions) env)))))
    (or (parse-in-actions (actions spec) env)
	(let ((arg (find name (arguments-list-arguments spec) :key key :test #'equal)))
	  (if arg 
	      (cons (cons arg (parse-argument arg rest)) env)
	      nil)))))

(defmacro null-cond (var null-value &body t-value)
  `(cond 
     ((null ,var) ,null-value)
     (t ,@t-value)))

(defun parse-arguments (args spec)
  (labels ((parse-cmd-argument (value)
	     (ecase (first value)
	       (:arg (list (second value) #'argument-name (make-condition 'wrong-argument-error 
									:string (second value))))
	       (:short (list (second value) #'argument-short-name (make-condition 'wrong-short-argument-error 
										  :char (second value))))
	       (:param (error 'wrong-argument-error :string (second value)))))
	   (do-parse-arguments (iter &optional env)
	     (null-cond (iterator-current iter) env
	       (destructuring-bind (name key error) (parse-cmd-argument (iterator-current iter))
		 (iterator-next iter)
		 (let ((env (%parse-argument name key iter spec env)))
		   (unless env
		     (error error))
		   (do-parse-arguments iter env))))))
    (do-parse-arguments (make-list-iterator :list (arguments-to-list args)))))

(defun find-argument-in-list (name list)
  (find name list :test #'equal :key #'(lambda (arg) (argument-name (first arg)))))

(defun argument-set-p (name args-list)
  (if (find-argument-in-list name args-list)  t nil))

(defun (setf argument-value) (value name args-list)
  (let ((arg (find-argument-in-list name args-list)))
    (if arg (setf (rest arg) value))))
      
(defun argument-value (name args-list)
  (let ((arg (find-argument-in-list name args-list)))
    (if arg (values (rest arg) t) (values nil nil))))
	