(in-package :burning-filesystem)

(defstruct (directory-path (:conc-name directory-) 
			   (:predicate %directory-path-p))
  host
  device
  path)

(defstruct (file-path (:conc-name file-) 
		      (:predicate %file-path-p))
  directory
  name
  type
  version)

;;
;; Macro defining a generic and default implementation signaling error.
;;

(defmacro def-noimpl-generic (name (fs &rest arguments))
  `(progn 
     (defgeneric ,name ,(cons fs arguments))
     (defmethod ,name ,(cons fs arguments)
       (declare (ignore ,@arguments))
       (flet ((fs-type (name)
		(if (symbolp name)
		    name
		    (type-of name))))
	 (error "Method ~a isn't implemented for filesystem type ~a." ',name (fs-type ,fs))))))

;;
;; Filesystem interface
;;

(def-noimpl-generic fs-path-from-string (fs string))
(def-noimpl-generic fs-path-to-string (fs path))

(def-noimpl-generic fs-file-exists-p (fs file))
(def-noimpl-generic fs-directory-exists-p (fs directory))

(def-noimpl-generic fs-list-directory (fs directory))

(def-noimpl-generic fs-current-directory (fs))
(def-noimpl-generic fs-home-directory (fs))

(def-noimpl-generic fs-delete-file (fs path))
(def-noimpl-generic fs-delete-directory (fs path))

(def-noimpl-generic fs-make-file (fs path))
(def-noimpl-generic fs-make-directory (fs path))

(def-noimpl-generic fs-as-file-path (fs directory-path))
(def-noimpl-generic fs-as-directory-path (fs file-path))

(defgeneric fs-open-file (fs path &key direction element-type if-exists if-does-not-exist))

(defgeneric ifs-open-stream (fs path direction element-type position))
(def-noimpl-generic fs-close-stream (fs path))

(def-noimpl-generic fs-file-length (fs path &optional element-type))

;;
;; Replaces existing file with new one
;;
;; Action is one of: :new-version, :rename, :rename-and-delete, :overwrite, :supersede

(defgeneric fs-file-replace (fs path action))

(defmethod fs-file-replace (fs path action)
  (ecase action
    ((:new-version :rename :rename-and-delete :supersede) (progn (fs-delete-file fs path)
								  (fs-make-file fs path)))
    (:overwrite t)))
  
;;
;; Error conditions
;;

(define-condition file-lock-error (error)
  ((path :initarg :path :reader file-lock-error-path)))
   
;;
;; Implementation
;;

(defmethod fs-open-file (fs path &key 
			 (direction :input) 
			 (element-type 'character) 
			 (if-exists :error)
			 (if-does-not-exist 'if-does-not-exist-default))
  (when (eq if-does-not-exist 'if-does-not-exist-default)
    (setf if-does-not-exist 
	  (ecase direction
	    (:input :error)
	    ((:output :io) (if (member if-exists '(:overwrite :append)) :error :create))
	    (:probe nil))))
  (let ((position :start))
    (labels ((check-if-exists ()
	       (ecase if-exists
		 (:error (error 'file-error :pathname path))
		 ((:new-version :rename :rename-and-delete :overwrite :supersede) 
		  (fs-file-replace fs path if-exists))
		 (:append (setf position :end))
		 ((nil) nil)))
	     (check-if-does-not-exist ()
	       (ecase if-does-not-exist
		 (:error (error 'file-error :pathname path))
		 (:create (fs-make-file fs path)
			  t)
		 ((nil) nil)))	       
	     (check-existance ()
	       (if (fs-file-exists-p fs path)
		   (or (member direction '(:input :probe))
		       (check-if-exists))
		   (check-if-does-not-exist))))
      (if (check-existance)
	  (fs-open-stream fs path direction element-type position)))))


  