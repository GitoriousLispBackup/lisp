(in-package #:burning-command-line)

;;
;; Argument base class
;;

(defclass argument ()
  ((name :initarg :name :reader argument-name)
   (short-name :initarg :short-name :reader argument-short-name)
   (description :initarg :description :reader argument-description)))

(defgeneric make-argument (class name &key short-name description &allow-other-keys))

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
;; Arguments specification
;;

(defclass arguments-spec () 
  ((name :initarg :name :reader arguments-list-name)
   (arguments :initarg :arguments :accessor arguments-list-arguments)))

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

(defun do-make-arguments-spec (name &key (help nil help-set-p) arguments)
  (let ((help (if help-set-p help (make-argument :flag "help" :description "Products this help message"))))
    (let ((arguments (if help (cons help arguments) arguments)))
      (labels ((have-same-names (args)
		 (cond
		   ((null args) nil)
		   ((have-same-name (first args) (rest args)))
		   (t (have-same-names (rest args))))))
	(let ((duplicate (have-same-names arguments)))
	  (if duplicate (error duplicate))))
      (make-instance 'arguments-spec :name name 
		     :arguments arguments))))

(defmacro make-arguments-spec (name-and-options &body argument-specs)
  (labels ((parse-option (spec)
	     (let ((name (if (listp spec) (first spec) spec)))
	       (ecase name
		 (:help `(:help (make-argument :flag ,@(rest spec))))
		 (:no-help `(:help nil)))))
	   (parse-options (opts)
	     (if (stringp opts)
		 (list opts)
		 `(,(first opts) ,@(reduce #'append (mapcar #'parse-option (rest opts)))))))
    `(do-make-arguments-spec ,@(parse-options name-and-options)
       :arguments (list ,@(mapcar #'(lambda (spec) `(make-argument ,@spec)) argument-specs)))))

(defun add-argument (arg list)
  (when (have-argument-p (argument-name arg) list)
    (error 'argument-already-exists-error :name (argument-name arg)))
  (push arg (arguments-list-arguments list)))

(defun argument (name list)
  (find name (arguments-list-arguments list) :test #'equal :key #'argument-name))

(defun have-argument-p (name list)
  (if (argument name list) t nil))

(defun string+ (&rest strings)
  (apply #'concatenate 'string strings))

(defun help-message (list)
  (labels ((usage-message (list)
	     (string+ (format nil "Usage:~%  ~a [ARGS]~%~%" (arguments-list-name list))))
	   (argument-message (arg)
	     (format nil "    --~a          ~a~%" (argument-name arg) (argument-description arg)))
	   (arguments-message (list)
	     (string+ (format nil "  Where ARGS are:~%")
		      (apply #'string+ (mapcar #'argument-message (arguments-list-arguments list)))
		      (format nil "~%"))))
    (string+ (usage-message list)
	     (arguments-message list))))

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

(defun parse-argument (value rest spec)
  (flet ((do-parse-argument (arg)
	   (values (cons arg t) rest))
	 (argument-by-short-name (name spec)
	   (find name (arguments-list-arguments spec) :key #'argument-short-name))) 
    (ecase (first value)
      (:arg (let ((arg (argument (second value) spec)))
	      (unless arg
		(error 'wrong-argument-error :string (second value)))
	      (do-parse-argument arg)))
      (:short (let ((arg (argument-by-short-name (second value) spec)))
		(unless arg
		  (error 'wrong-short-argument-error :char (second value)))
		(do-parse-argument arg)))
      (:param (error 'wrong-argument-error :string (second value))))))

(defmacro null-cond (var null-value &body t-value)
  `(cond 
     ((null ,var) ,null-value)
     (t ,@t-value)))

(defun parse-arguments (args spec)
  (labels ((do-parse-arguments (list)
	     (null-cond list nil
	       (multiple-value-bind (arg rest) (parse-argument (first list) (rest list) spec)
		 (cons arg (do-parse-arguments rest))))))
    (do-parse-arguments (arguments-to-list args))))

(defun argument-set-p (name args-list)
  (if (find name args-list :test #'equal :key #'(lambda (arg) (argument-name (first arg)))) t nil))
      
