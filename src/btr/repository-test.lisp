(in-package #:burning-btr-test)

(defun print-repository (repo &optional stream)
  (burning-btr::print-repository repo stream))

(defcase repository-test)

;;
;;Simple repository
;;

(deftest repository-test creating-repository ()
  (let ((repository (make-repository)))
    (!eq (entities repository) ())
    (!equal (repository-version repository) "0.1")))

(defun echo (&rest lines)
  (make-string-input-stream 
   (with-output-to-string (stream)
     (mapc #'(lambda (line) (write-line line stream)) lines))))

(deftest repository-test saving-repository ()
  (!equal (print-repository (make-repository))
	  (lines "<repository version=\"0.1\"/>")))

(defun read-repository (stream)
  (burning-btr::read-repository stream))

(deftest repository-test loading-repository ()
  (let ((repository (read-repository (echo "<repository version=\"a.version\"/>"))))
    (!eq (entities repository) ())
    (!equal (repository-version repository) "a.version")))

(deftest repository-test loading-xml-with-wrong-name ()
  (!condition (read-repository (echo "<repo/>"))
	      wrong-repository-node-name
	      (wrong-repository-node-name-name "repo")))

(deftest repository-test loading-xml-without-version ()
  (!condition (read-repository (echo "<repository/>"))
	      repository-api-version-does-not-specified))

;;
;; Repository with units
;;

(deftest repository-test adding-unit ()
  (let ((unit (make-unit "unit" :files '("name1" "name.2" "third name"))))
    (!equal (unit-name unit) "unit")
    (!equal (unit-files unit) '("name1" "name.2" "third name"))
    (let ((repository (make-repository)))
      (add-entity unit repository)
      (!equal (entities repository) `(,unit)))))

(deftest repository-test removing-unit ()
  (let ((unit (make-unit "unit"))
	(repo (make-repository)))
    (add-entity unit repo)
    (remove-entity unit repo)
    (!eq (entities repo) ())))

(deftest repository-test units-with-same-name ()
  (let ((repo (make-repository))
	(unit (make-unit "unit")))
    (add-entity unit repo)
    (!condition (add-entity unit repo)
		unit-with-same-name-already-exists
		(unit-with-same-name-already-exists-name "unit"))))

(deftest repository-test working-with-several-units ()
  (let ((unit1 (make-unit "unit1"))
	(unit2 (make-unit "unit2"))
	(unit3 (make-unit "unit3"))
	(repo (make-repository)))
    (add-entity unit1 repo)
    (add-entity unit2 repo)
    (add-entity unit3 repo)
    (!equal (entities repo) (list unit1 unit2 unit3))
    (remove-entity unit2 repo)
    (!equal (entities repo) (list unit1 unit3))
    (remove-entity unit3 repo)
    (!equal (entities repo) (list unit1))))

(deftest repository-test print-units ()
  (let ((unit1 (make-unit "unit1" :files '("file1" "file2")))
	(unit2 (make-unit "unit2" :files '("f1")))
	(unit3 (make-unit "unit3" :files '("file-one" "file-two" "file-three")))
	(repo (make-repository)))
    (add-entity unit1 repo)
    (add-entity unit2 repo)
    (add-entity unit3 repo)
    (!equal (print-repository repo)
	    (lines "<repository version=\"0.1\">"
		   "  <unit name=\"unit1\">"
		   "    <file name=\"file1\"/>"
		   "    <file name=\"file2\"/>"
		   "  </unit>"
		   "  <unit name=\"unit2\">"
		   "    <file name=\"f1\"/>"
		   "  </unit>"
		   "  <unit name=\"unit3\">"
		   "    <file name=\"file-one\"/>"
		   "    <file name=\"file-two\"/>"
		   "    <file name=\"file-three\"/>"
		   "  </unit>"
		   "</repository>"))))

;;parsing units

;;;directories


			 
			 