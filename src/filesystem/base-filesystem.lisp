(in-package :burning-filesystem)

(defstruct (directory-path (:conc-name directory-))
  filesystem
  host
  device
  path)

(defstruct (file-path (:conc-name file-))
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

(def-noimpl-generic fs-open-input-stream (fs path &optional element-type))
(def-noimpl-generic fs-open-output-stream (fs path &key element-type position))
(def-noimpl-generic fs-open-io-stream (fs path &key element-type position))

(def-noimpl-generic fs-close-stream (fs path))

(defmethod fs-open-file (fs path &key 
			 (direction :input) 
			 (element-type 'character) 
			 (if-exists :error)
			 (if-does-not-exist :default))
  (ecase direction
    (:output (unless (fs-file-exists-p fs path)
	       (fs-make-file fs path))
	     (fs-open-output-stream fs path :element-type element-type :position :start))))
  