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

;;
;; Checks that path exist (second value) and represent correct filesystem object.
;;

(def-noimpl-generic fs-file-exists-p (fs file))
(def-noimpl-generic fs-directory-exists-p (fs directory))

(def-noimpl-generic fs-list-directory (fs directory))

(def-noimpl-generic fs-current-directory (fs))
(def-noimpl-generic fs-home-directory (fs))

(def-noimpl-generic fs-delete-file (fs path))
(def-noimpl-generic fs-delete-directory (fs path))

(def-noimpl-generic fs-make-file (fs path))
(def-noimpl-generic fs-make-directory (fs path))

(def-noimpl-generic fs-open-file (fs path &key direction element-type if-exists if-does-not-exist external-format))
(def-noimpl-generic fs-close-file (fs path))

