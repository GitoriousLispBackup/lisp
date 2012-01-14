(in-package #:burning-btr-test)

(defcase action-test)

(defmacro def-action-test (name &body body)
  `(deftest action-test ,name ()
     (let ((*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-action-test running-without-action
  (!condition (btr-run '())
	      no-action-specified-error))

(def-action-test creation-test 
  (btr-run '("--create"))
  (!t (path-exists-p (path-from-string "/work/.btr/repository.conf")))
  (!equal (read-file (path-from-string "/work/.btr/repository.conf"))
	  (lines "<repository version=\"0.1\"/>")))

(def-action-test creating-in-specified-directory
  (btr-run '("--create" "-R" "/home"))
  (!null (path-exists-p (path-from-string "/work/.btr/"))))

(def-action-test creating-with-existing-directory
  (write-repository (make-repository) (current-directory))
  (!condition (btr-run '("--create" "-R" "/work/bla"))
	      repository-already-exists-error
	      (repository-already-exists-error-path (current-directory) :test path=)))

(def-action-test creating-in-wrong-directory
  (!condition (btr-run '("--create" "-R" "/bla1/bla2"))
	      wrong-directory-path-argument-error
	      (cmd-parsing-error-argument "repository")
	      (wrong-key-value-error-value "/bla1/bla2")))

;;running with several actions


    

    
    
    
