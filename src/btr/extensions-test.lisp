(in-package #:burning-btr-test)

(defcase extension-test)

(define-btr-class my-repository repository
  (status :initform "unspecified")
  (name :xml-name "a-name" :initform 1 :accessor mr-name))

(defmacro def-extension-test (name &body body)
  `(deftest extension-test ,name ()
     (let ((*repository-class* 'my-repository))
       ,@body)))

(def-extension-test creating-repository
  (let ((repo (make-repository)))
    (!eq (mr-name repo) 1)
    (!equal (slot-value repo 'status) "unspecified")
    (!equal (print-repository repo)
	    (lines "<repository version=\"0.1\" a-name=1 status=\"unspecified\"/>"))))
    
    
    