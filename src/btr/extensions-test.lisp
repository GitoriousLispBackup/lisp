(in-package #:burning-btr-test)

(defcase extension-test)

(define-btr-class my-repository repository
  (status :initform "unspecified")
  (name :xml-name "a_name" :initform 1 :accessor mr-name))

(define-btr-class my-unit unit
  (version :initform 0 :reader mu-version)
  (owner :initform "unknown" :reader mu-owner))

(defmacro def-extension-test (name &body body)
  `(deftest extension-test ,name ()
     (let ((*repository-class* 'my-repository)
	   (*unit-class* 'my-unit)
	   (*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-extension-test creating-repository
  (let ((repo (make-repository)))
    (!eq (mr-name repo) 1)
    (!equal (slot-value repo 'status) "unspecified")
    (!equal (print-repository repo)
	    (lines "<repository version=\"0.1\" a_name=1 status=\"unspecified\"/>"))))

(def-extension-test parsing-repository 
  (let ((repo (read-repository (echo "<repository version=\"blabla\" a_name=101 status=\"a status\"/>"))))
    (!equal (repository-version repo) "blabla")
    (!eq (mr-name repo) 101)
    (!equal (slot-value repo 'status) "a status")))

(def-extension-test creating-units 
  (let ((repo (make-repository))
	(unit1 (make-unit "u1"))
	(unit2 (make-unit "u2"))
	(group (make-group "g")))
    (!eq (mu-version unit1) 0)
    (!equal (mu-owner unit1) "unknown")
    (add-entity unit1 repo)
    (add-entity group repo)
    (add-entity unit2 group)
    (!equal (print-repository repo)
	    (lines "<repository version=\"0.1\" a_name=1 status=\"unspecified\">"
		   "  <unit name=\"u1\" owner=\"unknown\" version=0/>"
		   "  <group name=\"g\">"
		   "    <unit name=\"u2\" owner=\"unknown\" version=0/>"
		   "  </group>"
		   "</repository>"))))

(def-extension-test parsing-units
  (let ((repo (read-repository (echo "<repository version=0 a_name=42 status=\"\">"
				     "  <unit name=\"u\" owner=\"burning\" version=101/>"
				     "</resository>"))))
    (!eq (length (entities repo)) 1)
    (let ((unit (first (entities repo))))
      (!equal (entity-name unit) "u")
      (!equal (mu-owner unit) "burning")
      (!eq (mu-version unit) 101))))

(def-extension-test lising-extended-units
  (init-repository)
  (write-file (path-from-string ".btr/repository.conf") 
	      (lines "<repository version=0 a_name=123 status=\"no\">"
		     "  <unit name=\"u\" owner=\"burning\" version=101/>"
		     "  <group name=\"g\">"
		     "    <unit name=\"u2\" owner=\"me\" version=202/>"
		     "  </group>"
		     "</repository>"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
	  (lines " Path  version  owner   "
		 ""
		 " g/u2  202      me      "
		 " u     101      burning "))
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines " Path  version  owner   "
		 ""
		 " g/   "
		 " u     101      burning ")))

(define-repository-function my-repository :create (repository args)
  (setf (mr-name repository) "created repository"))

(def-extension-test initialize-function-extension 
  (btr-run '("--create"))
  (!equal (read-file (path-from-string ".btr/repository.conf"))
	  (lines "<repository version=\"0.1\" a_name=\"created repository\" status=\"unspecified\"/>")))
