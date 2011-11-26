(in-package #:burning-filesystem-test)

(defcase ui-test)

(defun %make-up (path fs)
  (burning-filesystem::make-ui-path :path path :filesystem fs))

(defun make-user-fp (path name type fs)
  (%make-up (make-fp (make-dp nil nil path) name type nil) fs))

(defun make-user-dp (path fs)
  (%make-up (make-dp nil nil path) fs))

(defmacro def-ui-test (name &body body)
  `(deftest ui-test ,name ()
     (let ((fs (make-virtual-filesystem)))
       (macrolet ((!path-eq (path expected &optional (fs 'fs))
		    `(!equalp ,path (burning-filesystem::make-ui-path :path ,expected :filesystem ,fs)))) 
	 ,@body))))

(def-ui-test simple-filename-reading
  (!path-eq (path-from-string "a.file.name" :fs fs)
	    (make-fp (make-dp nil nil '(:relative)) "a.file" "name" nil)))

(def-ui-test filename-with-directory-reading 
  (!path-eq (path-from-string "/path/to/directory/file.name" :fs fs)
	    (make-fp (make-dp nil nil '(:absolute "path" "to" "directory")) "file" "name" nil)))

(def-ui-test wrong-filename-reading
  (!condition (path-from-string "/path/" :type :file :fs fs) wrong-filename-error
	      (wrong-filename-error-string "/path/")))

(def-ui-test simple-directory-name-reading
  (!path-eq (path-from-string "tmp/" :fs fs)
	    (make-dp nil nil '(:relative "tmp")) fs))

(def-ui-test absolute-directory-name-reading
  (!path-eq (path-from-string "/a.root.directory/" :fs fs)
	    (make-dp nil nil '(:absolute "a.root.directory"))))

(def-ui-test complex-relative-directory-name-reading
  (!path-eq (path-from-string "dir1/dir2/dir3/my.dir/" :fs fs)
	    (make-dp nil nil '(:relative "dir1" "dir2" "dir3" "my.dir"))))
	    
(def-ui-test complex-absolute-directory-name-reading
  (!path-eq (path-from-string "/root/dir1/dir2/the.last.dir/" :fs fs)
	    (make-dp nil nil '(:absolute "root" "dir1" "dir2" "the.last.dir"))))

(def-ui-test directory-name-in-file-form-reading
  (!path-eq (path-from-string "/root/dir1/dir2/dir3" :type :directory :fs fs)
	    (make-dp nil nil '(:absolute "root" "dir1" "dir2" "dir3"))))

(def-ui-test simple-filename-writing
  (!equal (path-to-string (make-user-fp '(:relative) "a.file.name" "an.ext" fs)) "a.file.name.an.ext"))

(def-ui-test complex-filename-writing
  (!equal (path-to-string (make-user-fp '(:absolute "dir1" "dir2" "dir3") "file" "ext" fs)) 
	  "/dir1/dir2/dir3/file.ext"))

(def-ui-test relative-filename-writing
  (!equal (path-to-string (make-user-fp '(:relative "dir1" "dir2" "dir3") "file" "ext" fs))
	  "dir1/dir2/dir3/file.ext"))

(def-ui-test relative-directory-name-writting
  (!equal (path-to-string (make-user-dp '(:relative "dir1" "dir2") fs)) "dir1/dir2/"))

(def-ui-test absolute-directory-name-writting
  (!equal (path-to-string (make-user-dp '(:absolute "a.dir" "b.dir") fs)) "/a.dir/b.dir/"))

(def-ui-test file-path-exists-p-test
  (let ((path1 (path-from-string "a.file" :fs fs))
	(inner-path1 (fs-path-from-string fs "a.file"))
	(path1-as-directory (path-from-string "a.file/" :fs fs))
	(path2 (path-from-string "/work/a.file" :fs fs))
	(path3 (path-from-string "/work/other.file" :fs fs)))
    (fs-make-file fs inner-path1)
    (!t (path-exists-p path1))
    (!t (path-exists-p path2))
    (!not (path-exists-p path1-as-directory))
    (!not (path-exists-p path3))))

;;directory path exists-p
;;path-exists-p in non-existing directory
	  