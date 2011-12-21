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
  ((arguments :accessor %arguments-list-arguments)
   (parent :initform nil :reader arguments-list-parent)))

(defun arguments-list-arguments (list)
  (let ((args (%arguments-list-arguments list)))
    (apply #'append
	   (remove-if #'group-p args)
	   (mapcar #'%arguments-list-arguments (remove-if-not #'group-p args)))))

(defgeneric arguments-list-name (list))

(defun check-arguments-list (list)
  (labels ((have-same-names (args)
	     (cond
	       ((null args) nil)
	       ((have-same-name (first args) (rest args)))
	       (t (have-same-names (rest args)))))
	   (have-group (arg)
	     (not (null (groups arg))))
	   (have-nested-groups (args)
	     (some #'have-group (remove-if-not #'group-p args))))
    (let ((nested-groups (have-nested-groups (%arguments-list-arguments list))))
      (if nested-groups (error "Nested groups not allowed")))
    (let ((duplicate (have-same-names (arguments-list-arguments list))))
      (if duplicate (error duplicate) t))))

(defun add-argument (arg list)
  (when (have-argument-p (argument-name arg) list)
    (error 'argument-already-exists-error :name (argument-name arg)))
  (push arg (%arguments-list-arguments list)))

(defun argument (name list)
  (find name (arguments-list-arguments list) :test #'equal :key #'argument-name))

(defun have-argument-p (name list)
  (or (some  #'(lambda (action) (have-argument-p name action)) (actions list))
      (if (argument name list) t nil)))

(defun string+ (&rest strings)
  (apply #'concatenate 'string strings))

(defgeneric arguments-list-description (list))

(defun help-message (list)
  (labels ((group-names (list)
	     (let ((group-names (mapcar #'(lambda (group) (format nil "~a" (group-name group)))
					(groups list))))
	       (reduce #'string+ group-names)))
	   (usage-message (list)
	     (string+ (if (string/= (arguments-list-description list) "")
			  (format nil "~a~%~%" (arguments-list-description list)) "")
		      (format nil "Usage:~%  ~a~a~%~%" (arguments-list-name list) (group-names list))))
	   (argument-name-message (arg)
	     (string+ (format nil "    --~a" (argument-name arg))
		      (if (argument-short-name arg) (format nil ",-~a" (argument-short-name arg)) "")
		      (if (typep arg 'key) (format nil " ~a" (type-value-name (key-argument-type arg))) "")))
	   (argument-description-message (arg)
	     (if (argument-description arg) (format nil "~a" (argument-description arg)) ""))
	   (arguments-message (list)
	       (string+ (format nil "  Where ~a are:~%" (arguments-list-name list))
			(if (> (length (arguments-list-arguments list)) 0)
			    (let ((names (mapcar #'argument-name-message (arguments-list-arguments list)))
				  (descs (mapcar #'argument-description-message (arguments-list-arguments list))))
			      (let ((space (+ 10 (apply #'max (mapcar #'length names)))))
				(apply #'string+ 
				       (mapcar #'(lambda (name desc) 
						   (format nil "~a~va~a~%" name (- space (length name)) "" desc))
					       names descs))))
			    "")
			(format nil "~%"))))
    (string+ (usage-message list)
	     (reduce #'string+ (mapcar #'arguments-message (groups list))))))

(defmethod initialize-instance :after ((obj arguments-list) &key (help nil help-set-p) arguments)
  (let ((default-group-arguments (remove-if #'group-p arguments)))
    (let ((help (if help-set-p help (make-argument :flag "help" :description "Products this help message"))))
      (if help (push help default-group-arguments)))
    (when (and (not (group-p obj)) (> (length default-group-arguments) 0))
      (setf default-group-arguments `(,(make-instance 'group 
						      :arguments default-group-arguments
						      :name "ARGS"
						      :help nil))))
    (setf (%arguments-list-arguments obj) (append default-group-arguments (remove-if-not #'group-p arguments))))
  (mapc #'(lambda (action) (setf (slot-value action 'parent) obj)) (actions obj))
  (check-arguments-list obj))

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

(defun find-key (name list)
  (cond
    ((null list) nil)
    ((eq (first list) name) (second list))
    (t (find-key name (rest list)))))

(defun remove-key (name list)
  (cond
    ((null list) nil)
    ((eq (first list) name) (remove-key name (rest (rest list))))
    (t (cons (first list) (remove-key name (rest list))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod parse-argument-spec ((class (eql :action)) spec)
    (let ((args (find-key :arguments spec))
	  (spec (remove-key :arguments spec)))
      `(make-arguments-list 'action ,spec ,args))))

(defmethod arguments-list-description ((list action))
  (argument-description list))

(defmethod arguments-list-name ((list action))
  (string+ (if (arguments-list-parent list) 
	       (format nil "~a " (arguments-list-name (arguments-list-parent list))) "")
   (format nil "--~a" (argument-name list))
	   (if (argument-short-name list) (format nil ",-~a" (argument-short-name list)) "")))

(defun actions (arg-list)
  (remove-if-not #'(lambda (arg) (typep arg 'action)) (arguments-list-arguments arg-list)))

(defun groups (arg-list)
  (remove-if-not #'group-p (%arguments-list-arguments arg-list)))

;;
;; Group class
;;

(defclass group (arguments-list)
  ((name :initarg :name :reader arguments-list-name)
   (args-min :initarg :args-min :initform 0 :reader group-args-min)
   (args-max :initarg :args-max :initform :infinity :reader group-args-max)))

(defun group-name (group)
  (let ((name (arguments-list-name group)))
    (flet ((optional-name ()
	     (format nil " [~a~@[ ...~]]" name (eq (group-args-max group) :infinity))))
      (string+ (if (> (group-args-min group) 0) (format nil " ~a" name) "")
	       (if (not (eq (group-args-min group) (group-args-max group)))
		   (optional-name) "")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod parse-argument-spec ((class (eql :group)) spec)
    (flet ((restriction-p (arg)
	     (member arg '(:one-max :one-only :one-min)))
	   (restriction-arg (arg)
	     (ecase arg
	       ((nil) nil)
	       (:one-max '(:args-max 1))
	       (:one-min '(:args-min 1))
	       (:one-only '(:args-min 1 :args-max 1)))))
      (let ((args (find-key :arguments spec))
	    (spec (remove-key :arguments spec)))
	(let ((restrictions (remove-if-not #'restriction-p spec))
	      (spec (remove-if #'restriction-p spec)))
	  (when (> (length restrictions) 1) (error "Too much restrictions for group."))
	  `(make-arguments-list 'group ,(append spec (restriction-arg (first restrictions)) '(:no-help))
				,args))))))

(defun group-p (arg)
  (typep arg 'group))

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
    (check-groups spec (do-parse-arguments (make-list-iterator :list (arguments-to-list args))))))

(define-condition too-few-arguments-in-group-set (error)
  ((group :initarg :group :reader too-few-arguments-in-group-set-group)))

(define-condition too-much-arguments-in-group-set (error)
  ((group :initarg :group :reader too-much-arguments-in-group-set-group)
   (arguments :initarg :arguments :reader too-much-arguments-in-group-set-arguments)))

(defun check-groups (spec env)
  (flet ((check-group (group)
	   (let ((set-arguments (remove-if-not #'(lambda (arg) (argument-set-p (argument-name arg) env))
					       (arguments-list-arguments group))))
	     (unless (eq (group-args-max group) :infinity)
	       (when (> (length set-arguments) (group-args-max group))
		 (error 'too-much-arguments-in-group-set 
			:group (arguments-list-name group)
			:arguments set-arguments)))
	     (when (< (length set-arguments) (group-args-min group))
	       (error 'too-few-arguments-in-group-set
		      :group (arguments-list-name group))))))
    (mapc #'check-group (groups spec))
    (mapc #'check-groups (actions spec) (mapcar #'(lambda (action) (argument-value (argument-name action) env)) 
						(actions spec)))
    env))

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
	