(in-package #:burning-btr)

;;
;; General actions running
;;

(defparameter *btr-arguments* (make-arguments-spec "btr"
				(:key "repository" :short-name #\R :type 'creatable-directory-path)))

(defparameter *btr-actions* ())

(define-condition repository-already-exists-error (error)
  ((path :initarg :path :reader repository-already-exists-error-path)))

(define-condition no-action-specified-error (error) ())
(define-condition too-much-actions-specified-error (error) ())

(defun btr-run (arguments)
  (let* ((args (parse-arguments arguments *btr-arguments*))
	 (path (if (argument-set-p "repository" args) (argument-value "repository" args) (current-directory))))
    (flet ((action-set-p (action)
	     (argument-set-p (first action) args)))
      (let ((set-actions (count-if #'action-set-p *btr-actions*)))
	(when (= set-actions 0)
	  (error 'no-action-specified-error))
	(when (> set-actions 1)
	  (error 'too-much-actions-specified-error))
	(funcall (rest (find-if #'action-set-p *btr-actions*)) path args)))))


(defmacro define-repository-action (name (&rest lambda-list) &body action)
  (let ((arg-sym (gensym)))
    `(let ((,arg-sym (make-argument :action ,name)))
       (progn (add-argument ,arg-sym *btr-arguments*)
	      (push (cons ,name
			  #'(lambda (,@lambda-list) ,@action))
		    *btr-actions*)))))

(defun repository-action (name)
  (argument name *btr-arguments*))

;;
;; Create action
;;

(define-repository-action "create" (path args)
  (when (repository-path path)
    (error 'repository-already-exists-error :path (repository-path path)))
  (let ((repository (make-repository)))
    (write-repository repository path)))
