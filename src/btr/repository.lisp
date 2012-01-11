(in-package #:burning-btr)

(defconstant *repository-api-version* "0.1")

(defgeneric entities (obj))

(defclass repository ()
  ((root :initarg :root :accessor repository-root)
   (version :initarg :version :reader repository-version)))

(defmethod entities ((obj repository))
  (repository-root obj))

(defun make-repository (&key (version *repository-api-version*))
  (make-instance 'repository :root () :version version))

(defun print-repository (repo stream)
  (flet ((do-print (stream) 
	   (xml-print (make-xml-node "repository" `(("version" ,(repository-version repo)))) stream)))
    (if stream
	(do-print stream)
	(with-output-to-string (stream)
	  (do-print stream)))))

(define-condition repository-api-version-not-specified (warning) ())

(defun version-from-xml (xml)
  (if (xml-attribute-p xml "version")
      (xml-attribute xml "version")
      (progn
	(warn 'repository-api-version-not-specified)
	"0.1b")))

(define-condition wrong-repository-node-name (error)
  ((name :initarg :name :reader wrong-repository-node-name-name)))

(defun read-repository (stream)
  (let ((xml (parse-xml stream)))
    (unless (equal (xml-node-name xml) "repository")
      (error 'wrong-repository-node-name :name (xml-node-name xml)))
    (make-repository :version (version-from-xml xml))))
