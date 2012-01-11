(in-package #:burning-btr)

(defconstant *repository-api-version* "0.1")

;;
;; Repository
;;

(defclass repository ()
  ((entities :initarg :entities :accessor entities)
   (version :initarg :version :reader repository-version)))

(defun make-repository (&key (version *repository-api-version*))
  (make-instance 'repository :entities () :version version))

(define-condition unit-with-same-name-already-exists (error)
  ((name :initarg :name :reader unit-with-same-name-already-exists-name)))

(defun add-entity (entity list)
  (when (find (unit-name entity) (entities list) :test #'equal :key #'unit-name)
    (error 'unit-with-same-name-already-exists :name (unit-name entity)))
  (setf (entities list) (nconc (entities list) (list entity))))

(defun remove-entity (entity list)
  (setf (entities list) (remove (unit-name entity) (entities list) :test #'equal :key #'unit-name)))

;;
;; Units
;;

(defclass unit ()
  ((name :initarg :name :reader unit-name)
   (files :initarg :files :reader unit-files)))

(defun make-unit (name &key files)
  (make-instance 'unit :name name :files files))

;;
;; Writing
;;

(defun repository-to-xml (repo)
  (labels ((file-to-xml (file)
	     (make-xml-node "file" (list `("name" ,file))))
	   (unit-to-xml (unit)
	     (make-xml-node "unit" (list `("name" ,(unit-name unit)))
			    (mapcar #'file-to-xml (unit-files unit)))))
    (make-xml-node "repository" 
		   `(("version" ,(repository-version repo)))
		   (mapcar #'unit-to-xml (entities repo)))))
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

(define-condition repository-api-version-does-not-specified (warning) ())

(defun version-from-xml (node)
  (if (xml-attribute-p node "version")
      (xml-attribute node "version")
      (progn (warn 'repository-api-version-does-not-specified))))

(define-condition wrong-repository-node-name (error)
  ((name :initarg :name :reader wrong-repository-node-name-name)))

(defun read-repository (stream)
  (let ((xml (parse-xml stream)))
    (unless (equal (xml-node-name xml) "repository")
      (error 'wrong-repository-node-name :name (xml-node-name xml)))
    (make-repository :version (version-from-xml xml))))


