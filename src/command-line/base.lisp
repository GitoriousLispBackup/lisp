(in-package #:burning-command-line)

(defstruct (argument-list (:constructor %make-argument-list))
  args)

(defun make-argument-list (&rest arguments)
  (%make-argument-list :args (mapcar #'(lambda (x) (cons x nil)) arguments)))

(defstruct (argument (:constructor %make-argument))
  name
  description)

(defun make-argument (name &key (description nil))
  (%make-argument :name name :description description))

(define-condition argument-already-exists-error (error)
  ((name :initarg :name :reader argument-already-exists-error-name)))

(defun add-argument (list arg)
  (if (not (have-argument-p list (argument-name arg)))
      (push (cons arg nil) (argument-list-args list))
      (error 'argument-already-exists-error :name (argument-name arg))))

(defun add-arguments (list &rest args)
  (mapc #'(lambda (arg) (add-argument list arg)) args))

(defun %find-argument (list name)
  (assoc name (argument-list-args list) :key #'argument-name :test #'equal))

(defun have-argument-p (list name)
  (if (%find-argument list name) t nil))

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
  
