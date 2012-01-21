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
	       `(update-argument #A,spec ,action-sym ,@(if group `(,group) nil)))))
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
    (initialize-repository repository args)
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
    (add-entity (let ((unit (make-unit name :files (list (path-to-string path)))))
		  (initialize-unit unit)
		  unit)
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

(defun entity-path-string (entity path)
  (flet ((entity-path (entity)
	     (let ((local-path (path+ path (path-from-string (entity-name entity) 
							     :type (if (typep entity 'unit) :file :directory)))))
	       (if (relative-path-p local-path)
		   (as-relative-path local-path (current-directory))
		   local-path))))
    (path-to-string (entity-path entity))))

(defun entity-strings (entity path)
  (let ((attributes (gethash *unit-class* *attributes*)))
    (let ((attribute-names (mapcar #'attribute-name-string attributes))
	  (attribute-values (mapcar #'(lambda (attribute) 
					(slot-value entity (attribute-name attribute))) 
				    attributes)))
      (let ((attributes (remove "name" (mapcar #'cons attribute-names attribute-values) :test #'equal :key #'first)))
	(list (cons "Path" (mapcar #'first attributes))
	      (cons (entity-path-string entity path) (mapcar #'rest attributes)))))))

(defun group-names (group path &optional recursive-p)
  (let ((units (remove-if-not #'(lambda (entity) (typep entity 'unit)) (entities group)))
	(groups (remove-if-not #'(lambda (entity) (typep entity 'group)) (entities group))))
    (flet ((group-strings (group)
	     (if recursive-p 
		 (group-names group 
			      (copy-path path :new-path (append (path-path path) (list (entity-name group))))
			      t)
		 (list (list "Path") (list (entity-path-string group path))))))
      (let ((group-attributes (mapcar #'group-strings groups))
	    (unit-attributes (mapcar #'(lambda (entity) (entity-strings entity path)) units)))
	(let ((group-header (first (first group-attributes)))
	      (unit-header (first (first unit-attributes)))
	      (group-attributes (mapcar #'rest group-attributes))
	      (unit-attributes (mapcar #'second unit-attributes)))
	  (append (cond (unit-header (list unit-header))
			(group-header (list group-header))
			(t nil))
		  (reduce #'append group-attributes)
		  unit-attributes))))))

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
    (labels ((print-with-padding (string padding)
	       (format t " ~va " padding string))
	     (padding (name strings)
	       (apply #'max (length name) (mapcar #'length strings)))
	     (map-max (fun f1 f2)
	       (cond
		 ((and (first f1) (first f2)) (cons (funcall fun (first f1) (first f2))
						    (map-max fun (rest f1) (rest f2))))
		 ((first f1) (cons (funcall fun (first f1) 0)
				   (map-max fun (rest f1) nil)))
		 (t nil)))
	     (print-line (name paddings)
	       (mapc #'print-with-padding name paddings)
	       (format t "~%")))
      (let* ((names (group-names repository path (argument-set-p "recursive" args)))
	     (paddings (make-list (apply #'max 0 (mapcar #'length names)) :initial-element 0)))
	(when names
	  (mapc #'(lambda (name) 
		    (setf paddings 
			  (map-max #'(lambda (padding string)
				       (max padding (length (format nil "~a" string))))
				   paddings name))) 
		names)
	  (print-line (first names) paddings))
	(format t "~%")
	(mapc #'(lambda (name) (print-line name paddings)) (rest names))
	nil))))

(define-action-arguments "ls" 
  (:positional "directory" 
	       :type 'existing-directory-path
	       :description "a directory to list"
	       :optional)
  (:flag "recursive" :short-name #\r :description "List all subdirectries"))

;;
;; Rm action
;;

(define-condition file-is-not-in-repository-error (error)
  ((path :initarg :path :reader file-is-not-in-repository-error-path)
   (repository-path :initarg :repository-path :reader file-is-not-in-repository-error-repository-path)))

(defun find-existing-group (path repository)
  (if (null path)
      repository
      (let ((group (find (first path) (entities repository) :test #'equal :key #'entity-name)))
	(if (and group (typep group 'group))
	    (find-existing-group (rest path) group)
	    nil))))

(define-condition directory-is-not-empty-error (error)
  ((path :initarg :path :reader directory-is-not-empty-error-path)
   (repository-path :initarg :repository-path :reader directory-is-not-empty-error-repository-path)))

(defun find-entity (path repository)
  (if (parent-path path)
      (let ((path (path-as-file path)))
	(let ((group (find-existing-group (rest (path-path path)) repository))
	      (name (path-to-string (copy-path path :new-path '(:relative)))))
	  (unless (and group (find name (entities group) :test #'equal :key #'entity-name))
	    (error 'file-is-not-in-repository-error :path path :repository-path (repository-path path)))
	  (values (find name (entities group) :test #'equal :key #'entity-name) group)))
      repository))

(defun remove-unit (path repository &optional recursive)
  (multiple-value-bind (entity group) (find-entity path repository)
    (when (and (typep entity 'group) (entities entity) (not recursive))
      (error 'directory-is-not-empty-error
	     :path (path-as-directory path)
	     :repository-path (repository-path path)))
    (release-unit entity)
    (remove-entity (entity-name entity) group)))

(defun to-relative-path (path repository-path)
  (let ((path (path- (as-absolute-path path) (as-absolute-path repository-path))))
    (unless path
      (error 'path-is-not-in-repository-error :repository-path repository-path :path path))
    path))

(define-repository-action "rm" (path args)
  (check-repository-exists path)
  (let ((repository (open-repository (repository-path path)))
	(files (argument-value "files" (argument-value "rm" args)))
	(args (argument-value "rm" args))
	(repository-path (repository-path path)))
    (mapc #'(lambda (path) (remove-unit path repository (argument-set-p "recursive" args)))
	  (mapcar #'(lambda (path) (to-relative-path path repository-path)) files))
    (write-repository repository repository-path)))

(define-action-arguments "rm"
  (:positional "files"
	       :type '(list existing-path)
	       :description "a paths of files or directiries to be removed")
  (:flag "recursive"
	 :short-name #\r
	 :description "recursive remove entity and it's subentites"))

;;
;; Update
;;

(defun mapc-units (fun &rest entities)
  (labels ((mapc-entity (entity)
	     (cond
	       ((typep entity 'unit) (funcall fun entity))
	       (t (mapc #'mapc-entity (entities entity))))))
    (mapc #'mapc-entity entities)))

(define-repository-action "update" (path args)
  (check-repository-exists path)
  (let ((repository (open-repository (repository-path path)))
	(paths (argument-value "path" args))
	(repository-path (repository-path path)))
    (unless paths
      (setf paths (list repository-path)))
    (apply #'mapc-units #'initialize-unit (mapcar #'(lambda (path) (find-entity path repository)) paths))
    (write-repository repository repository-path)))

(define-action-arguments "update" (:positional "path" 
					       :type '(list existing-path) 
					       :description "path to update"
					       :optional))
