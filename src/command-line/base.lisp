(in-package #:burning-command-line)

;;
;; Argument structure
;;

(defstruct (argument (:constructor %make-argument))
  name
  description
  short-name)

(defun make-argument (name &key (description nil) (short nil))
  (%make-argument :name name :description description :short-name short))

;;
;; Argument list structure
;;

(defstruct (argument-list (:constructor %make-argument-list))
  args)

(defun make-argument-list (&key arguments (help nil help-p))
  (%make-argument-list 
   :args (mapcar #'(lambda (x) (cons x nil))
		 (let ((help-arg (if help-p help 
				     (make-argument "help" :description "Products this help message"))))
		     (if help-arg (cons help-arg arguments) arguments)))))

(define-condition argument-already-exists-error (error)
  ((name :initarg :name :reader argument-already-exists-error-name)))

(define-condition short-name-already-exists-error (error)
  ((char :initarg :char :reader short-name-already-exists-error-char)))

(defun have-argument-p (list name)
  (if (%find-argument list name) t nil))

(defun have-short-name-p (list char)
  (if char
      (assoc char (argument-list-args list) :key #'argument-short-name)))

(defun add-argument (list arg)
  (when (have-argument-p list (argument-name arg))
    (error 'argument-already-exists-error :name (argument-name arg)))
  (when (have-short-name-p list (argument-short-name arg))
    (error 'short-name-already-exists-error :char (argument-short-name arg)))
  (push (cons arg nil) (argument-list-args list)))

(defun add-arguments (list &rest args)
  (mapc #'(lambda (arg) (add-argument list arg)) args))

(defun %find-argument (list name)
  (assoc name (argument-list-args list) :key #'argument-name :test #'equal))


(define-condition argument-not-defined-error (error)
  ((name :initarg :name :reader argument-not-defined-error-name)))

(defun argument-set-p (list name)
  (let ((arg (%find-argument list name)))
    (if arg 
	(if (rest arg) t nil)
	(error 'argument-not-defined-error :name name))))

(defun argument (list name)
  (let ((arg (%find-argument list name)))
    (if arg
	(first arg)
	(error 'argument-not-defined-error :name name))))

(defun help-message (list)
  (labels ((usage-string ()
	     (format nil "Usage:~%"))
	   (arguments-string ()
	     (format nil "  [ARGS]~%~%"))
	   (where-string ()
	     (format nil "  Where ARGS are:~%"))
	   (name-string (arg)
	     (concatenate 'string 
			  (format nil "    --~a" (argument-name arg))
			  (if (argument-short-name arg) (format nil ",-~a" (argument-short-name arg)) "")))
	   (description-string (arg)
	     (argument-description arg))
	   (describe-strings (args)
	     (let ((names (mapcar #'name-string args))
		   (descs (mapcar #'description-string args)))
	       (let ((name-length (+ 10 (apply #'max (mapcar #'length names)))))
		 (apply #'concatenate 'string
			(mapcar #'(lambda (name desc) (format nil "~a~va~a~%" 
							      name 
							      (- name-length (length name)) 
							      "" desc))
				names descs)))))
	   (end-string ()
	     (format nil "~%")))
    (concatenate 'string
		 (usage-string)
		 (arguments-string)
		 (where-string)
		 (describe-strings (mapcar #'first (argument-list-args list)))
		 (end-string))))