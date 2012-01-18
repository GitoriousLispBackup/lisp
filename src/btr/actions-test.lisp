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

(def-action-test adding-unit
  (write-file (path-from-string "file") "a test one")
  (btr-run '("--create"))
  (btr-run '("--add" "file"))
  (!equal (read-file (path-from-string ".btr/repository.conf"))
	  (lines "<repository version=\"0.1\">"
		 "  <unit name=\"file\">"
		 "    <file name=\"file\"/>"
		 "  </unit>"
		 "</repository>")))

(def-action-test adding-several-units
  (write-file (path-from-string "file") "")
  (write-file (path-from-string "file2") "")
  (write-file (path-from-string "file3") "")
  (btr-run '("--create"))
  (btr-run '("--add" "file" "file2" "file3"))
  (!equal (read-file (path-from-string ".btr/repository.conf"))
	  (lines "<repository version=\"0.1\">"
		 "  <unit name=\"file\">"
		 "    <file name=\"file\"/>"
		 "  </unit>"
		 "  <unit name=\"file2\">"
		 "    <file name=\"file2\"/>"
		 "  </unit>"
		 "  <unit name=\"file3\">"
		 "    <file name=\"file3\"/>"
		 "  </unit>"
		 "</repository>")))

(def-action-test adding-units-without-repository 
  (make-file (path-from-string "file"))
  (!condition (btr-run '("--add" "file"))
	      repository-does-not-exist-error
	      (repository-does-not-exist-error-path (path-from-string "") :test path=)))

(def-action-test running-with-several-actions
  (make-file (path-from-string "file"))
  (!condition (btr-run '("--create" "--add" "file"))
	      too-much-actions-specified-error
	      (too-much-actions-specified-error-actions '("add" "create"))))

(def-action-test adding-groups
  (make-file (path-from-string "dir/file1") :recursive t)
  (make-file (path-from-string "dir/file2") :recursive t)
  (make-file (path-from-string "dir2/file") :recursive t)
  (btr-run '("--create"))
  (btr-run '("--add" "dir/file1" "dir/file2" "dir2/file"))
  (!equal (read-file (path-from-string ".btr/repository.conf"))
	  (lines "<repository version=\"0.1\">"
		 "  <group name=\"dir\">"
		 "    <unit name=\"file1\">"
		 "      <file name=\"dir/file1\"/>"
		 "    </unit>"
		 "    <unit name=\"file2\">"
		 "      <file name=\"dir/file2\"/>"
		 "    </unit>"
		 "  </group>"
		 "  <group name=\"dir2\">"
		 "    <unit name=\"file\">"
		 "      <file name=\"dir2/file\"/>"
		 "    </unit>"
		 "  </group>"
		 "</repository>")))

(def-action-test adding-wrong-files
  (make-file (path-from-string "/home/file"))
  (btr-run '("--create"))
  (!condition (btr-run '("--add" "/home/file"))
	      path-is-not-in-repository-error
	      (path-is-not-in-repository-error-path (path-from-string "/home/file") :test path=)
	      (path-is-not-in-repository-error-repository-path (path-from-string "/work/") :test path=)))

(defun make-file-from-string (string)
  (make-file (path-from-string string) :recursive t))

(defmacro with-standard-output-to-string (&body body)
  `(let ((*standard-output* (make-string-output-stream)))
     ,@body
     (get-output-stream-string *standard-output*)))

(def-action-test simple-listing
  (mapcar #'make-file-from-string '("file1" "a-very-long-file-name" "file3"))
  (btr-run '("--create"))
  (btr-run '("--add" "file1" "a-very-long-file-name" "file3"))
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines " Name                  " 
		 "" 
		 " file1                 " 
		 " a-very-long-file-name " 
		 " file3                 ")))

(def-action-test listing-subdirectory 
  (mapcar #'make-file-from-string '("dir/file1" "dir/file2"))
  (btr-run '("--create"))
  (btr-run '("--add" "dir/file1" "dir/file2"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "dir")))
	  (lines " Name      "
		 ""
		 " dir/file1 "
		 " dir/file2 ")))

(def-action-test listing-groups
  (mapcar #'make-file-from-string '("file1" "dir/file2"))
  (btr-run '("--create"))
  (btr-run '("--add" "file1" "dir/file2"))
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines " Name  "
		 ""
		 " dir/  "
		 " file1 ")))

(def-action-test listing-recursive 
  (mapcar #'make-file-from-string '("file1" "dir1/file2" "dir1/file3" "dir2/file4"))
  (btr-run '("--create"))
  (btr-run '("--add" "file1" "dir1/file2" "dir1/file3" "dir2/file4"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
	  (lines " Name       "
		 ""
		 " dir1/file2 "
		 " dir1/file3 "
		 " dir2/file4 "
		 " file1      ")))

(def-action-test listing-from-other-directory
  (mapcar #'make-file-from-string '("file1" "dir/file2"))
  (btr-run '("--create"))
  (btr-run '("--add" "file1" "dir/file2"))
  (vfs-cd (path-from-string "dir/"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
	  (lines " Name     "
		 ""
		 " file2    "
		 " ../file1 ")))

(def-action-test listing-from-outside 
  (mapcar #'make-file-from-string '("file1" "dir/file2"))
  (btr-run '("--create"))
  (btr-run '("--add" "file1" "dir/file2"))
  (vfs-cd (path-from-string "/home/"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r" "-R" "/work")))
	  (lines " Name            "
		 ""
		 " /work/dir/file2 "
		 " /work/file1     ")))

;;removing tests 
;;removing groups



    

    
    
    
