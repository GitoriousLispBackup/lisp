(in-package #:burning-btr)

(defconstant *repository-api-version* "0.1")

;;
;; Entity
;;

(defclass entity () 
  ((name :initarg :name :reader entity-name)))

;;
;; Entities group
;;

(defclass entities-group ()
  ((entities :initarg :entities :accessor entities)))

(define-condition entity-with-same-name-already-exists (error)
  ((name :initarg :name :reader entity-with-same-name-already-exists-name)))

(defun add-entity (entity list)
  (when (find (entity-name entity) (entities list) :test #'equal :key #'entity-name)
    (error 'entity-with-same-name-already-exists :name (entity-name entity)))
  (setf (entities list) (nconc (entities list) (list entity))))

(defun remove-entity (entity list)
  (setf (entities list) (remove (entity-name entity) (entities list) :test #'equal :key #'entity-name)))


;;
;; Repository
;;

(defclass repository (entities-group)
  ((version :initarg :version :reader repository-version)))

(defun make-repository (&key (version *repository-api-version*) entities)
  (make-instance 'repository :entities entities :version version))

;;
;; Units
;;

(defclass unit (entity)
  ((files :initarg :files :reader unit-files)))

(defun make-unit (name &key files)
  (make-instance 'unit :name name :files files))

;;
;; Groups
;;

(defclass group (entity entities-group) ())

(defun make-group (name)
  (make-instance 'group :name name :entities ()))

;;
;; Writing
;;

(defgeneric entity-to-xml (entity))

(defmethod entity-to-xml ((unit unit))
  (flet ((file-to-xml (file)
	     (make-xml-node "file" (list `("name" ,file)))))
    (make-xml-node "unit" (list `("name" ,(entity-name unit)))
		   (mapcar #'file-to-xml (unit-files unit)))))

(defmethod entity-to-xml ((group group))
  (make-xml-node "group" `(("name" ,(entity-name group))) (mapcar #'entity-to-xml (entities group))))

(defun repository-to-xml (repo)
  (make-xml-node "repository" 
		 `(("version" ,(repository-version repo)))
		 (mapcar #'entity-to-xml (entities repo))))

(defun print-repository (repo stream)
  (flet ((do-print (stream) 
	   (xml-print (repository-to-xml repo) stream)))
    (if stream
	(do-print stream)
	(with-output-to-string (stream)
	  (do-print stream)))))

;;
;; Reading
;;

(defun check-node-name (name node)
  (unless (equal (xml-node-name node) name)
    (error 'wrong-xml-node-name :expected name :got (xml-node-name node)))
  t)

(define-condition missed-xml-attribute-warning (warning)
  ((node-name :initarg :node-name :reader missed-xml-attribute-warning-node-name)
   (name :initarg :name :reader missed-xml-attribute-warning-name)))
  
(defun attribute-value (name node &optional default-value)
  (if (xml-attribute-p node name)
      (xml-attribute node name)
      (progn
	(warn 'missed-xml-attribute-warning :node-name (xml-node-name node) :name name)
	default-value)))

(define-condition wrong-xml-node-name (error)
  ((expected :initarg :expected :reader wrong-xml-node-name-expected)
   (got :initarg :got :reader wrong-xml-node-name-got)))

(defgeneric parse-entity-from-xml (type node))

(defmethod parse-entity-from-xml ((type (eql 'unit)) node)
  (flet ((file-from-xml (node)
	   (check-node-name "file" node)
	   (attribute-value "name" node)))
    (make-unit (attribute-value "name" node) :files (mapcar #'file-from-xml (xml-childs node)))))

(defmethod parse-entity-from-xml ((type (eql 'group)) node)
  (let ((group (make-group (attribute-value "name" node))))
    (mapc #'(lambda (node) (add-entity (entity-from-xml node) group)) (xml-childs node))
    group))

(defun entity-from-xml (node)
  (cond
    ((equal (xml-node-name node) "unit") (parse-entity-from-xml 'unit node))
    ((equal (xml-node-name node) "group") (parse-entity-from-xml 'group node))
    (t (error 'wrong-xml-node-name :got (xml-node-name node) :expected '("unit" "group")))))

(defun read-repository (stream)
  (let ((xml (parse-xml stream)))
    (check-node-name "repository" xml)
    (make-repository :version (attribute-value "version" xml *repository-api-version*)
		     :entities (mapcar #'entity-from-xml (xml-childs xml)))))


