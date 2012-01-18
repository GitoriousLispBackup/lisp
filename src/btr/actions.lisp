(in-package #:burning-btr)

;;
;; General actions running
;;

(defparameter *btr-arguments* (make-arguments-spec "btr"
				(:key "repository" 
				      :short-name #\R 
				      :description "path to repository"
				      :type 'creatable-directory-path)))

(defparameter *btr-actions* ())

(define-condition no-action-specified-error (error) ())

(define-condition too-much-actions-specified-error (error) 
  ((actions :initarg :actions :reader too-much-actions-specified-error-actions)))

(defun btr-run (arguments)
  (let* ((args (parse-arguments arguments *btr-arguments*))
	 (path (if (argument-set-p "repository" args) (argument-value "repository" args) (path-from-string ""))))
    (flet ((action-set-p (action)
	     (argument-set-p (first action) args)))
      (let ((set-actions (count-if #'action-set-p *btr-actions*)))
	(when (= set-actions 0)
	  (error 'no-action-specified-error))
	(when (> set-actions 1)
	  (error 'too-much-actions-specified-error 
		 :actions (mapcar #'first (remove-if-not #'action-set-p *btr-actions*))))
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

(defmacro define-action-arguments (name &body argument-specs)
  (let ((action-sym (gensym)))
    (flet ((add-argument-code (spec)
	     (let ((spec (remove-key-argument :group spec))
		   (group (key-argument-value :group spec)))
	       `(add-argument #A,spec ,action-sym ,@(if group `(,group) nil)))))
      `(let ((,action-sym (repository-action ,name)))
	 ,@(mapcar #'add-argument-code argument-specs)))))

;;
;; Create action
;;

(define-condition repository-does-not-exist-error (error)
  ((path :initarg :path :reader repository-does-not-exist-error-path)))

(defun check-repository-exists (path)
  (unless (repository-path path)
    (error 'repository-does-not-exist-error :path path))
  t)

(define-condition repository-already-exists-error (error)
  ((path :initarg :path :reader repository-already-exists-error-path)))

(defun check-repository-does-not-exist (path)
  (when (repository-path path)
    (error 'repository-already-exists-error :path (repository-path path)))
  t)

(define-repository-action "create" (path args)
  (check-repository-does-not-exist path)
  (let ((repository (make-repository)))
    (write-repository repository path)))

;;
;; Add action
;;

(defun find-group (path base-group)
  (if (null path) base-group
      (let ((name (first path)))
	(unless (find name (entities base-group) :key #'entity-name :test #'equal)
	  (add-entity (make-group name) base-group))
	(find-group (rest path) (find name (entities base-group) :key #'entity-name :test #'equal)))))

(defun add-unit (path repository)
  (let ((name (path-to-string (copy-path path :new-path '(:relative)))))
    (add-entity (make-unit name :files (list (path-to-string path)))
		(find-group (rest (path-path path)) repository))))

(define-condition path-is-not-in-repository-error (error)
  ((path :initarg :path :reader path-is-not-in-repository-error-path)
   (repository-path :initarg :repository-path :reader path-is-not-in-repository-error-repository-path)))

(define-repository-action "add" (path args)
  (check-repository-exists path)
  (let ((repository (open-repository (repository-path path)))
	(args (argument-value "add" args)))
    (flet ((relative-path (arg)
	     (let ((result (path- arg (as-absolute-path (repository-path path)))))
	       (unless result
		 (error 'path-is-not-in-repository-error :path arg :repository-path path))
	       result)))
      (mapc #'(lambda (arg) (add-unit (relative-path (as-absolute-path arg)) repository))
	    (argument-value "files" args)))
    (write-repository repository (repository-path path))))

(define-action-arguments "add" 
  (:positional "files" 
	       :type '(list existing-file-path)
	       :description "adds file to repository"))

;;
;; Ls action
;;

(defun group-names (group path &optional recursive-p)
  (labels ((entity-path (entity)
	     (let ((local-path (path+ path (path-from-string (entity-name entity) 
							     :type (if (typep entity 'unit) :file :directory)))))
	       (if (relative-path-p local-path)
		   (as-relative-path local-path (current-directory))
		   local-path)))
	   (entity-string (entity)
	     (path-to-string (entity-path entity))))
    (let ((units (remove-if-not #'(lambda (entity) (typep entity 'unit)) (entities group)))
	  (groups (remove-if-not #'(lambda (entity) (typep entity 'group)) (entities group))))
      (flet ((group-strings (group)
	       (if recursive-p 
		   (group-names group 
				(copy-path path :new-path (append (path-path path) (list (entity-name group))))
				t)
		   (list (entity-string group)))))
	(append (reduce #'append (mapcar #'group-strings groups))
		(mapcar #'entity-string units))))))

(define-repository-action "ls" (path args)
  (check-repository-exists path)
  (let ((repository (open-repository (repository-path path)))
	(args (argument-value "ls" args))
	(path (repository-path path)))
    (when (argument-set-p "directory" args)
      (let ((list-path (path- (as-absolute-path (argument-value "directory" args))
			      (as-absolute-path (repository-path path)))))
	(setf repository (find-group (rest (path-path list-path)) repository))
	(setf path (path+ path list-path))))
    (flet ((print-with-padding (string padding)
	     (format t " ~va ~%" padding string)))
      (let* ((names (group-names repository path (argument-set-p "recursive" args)))
	     (padding (apply #'max (mapcar #'length names))))
	(format t " ~va ~%~%" padding "Name")
	(mapc #'(lambda (string) (print-with-padding string padding)) names)
	nil))))

(define-action-arguments "ls" 
  (:positional "directory" 
	       :type 'existing-directory-path
	       :description "a directory to list"
	       :optional)
  (:flag "recursive" :short-name #\r :description "List all subdirectries"))
