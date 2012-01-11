(in-package #:burning-btr-test)

(defun print-repository (repo &optional stream)
  (burning-btr::print-repository repo stream))

(defcase repository-test)

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

;;loading xml with wrong name
;;loading xml without version