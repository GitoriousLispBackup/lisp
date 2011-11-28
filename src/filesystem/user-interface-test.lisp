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

(def-ui-test directory-path-exists-p-test
  (let ((path1 (path-from-string "directory/" :fs fs))
	(inner-path1 (fs-path-from-string fs "directory/"))
	(absolute-path1 (path-from-string "/work/directory/" :fs fs))
	(path1-fileform (path-from-string "/work/directory" :fs fs))
	(path2 (path-from-string "other.directory/" :fs fs)))
    (fs-make-directory fs inner-path1)
    (!t (path-exists-p path1))
    (!t (path-exists-p absolute-path1))
    (!not (path-exists-p path1-fileform))
    (!not (path-exists-p path2))))

(def-ui-test path-exists-p-in-non-existing-directory
  (let ((path1 (path-from-string "dir/file" :fs fs))
	(path2 (path-from-string "dir/dir/" :fs fs)))
    (!not (path-exists-p path1))
    (!not (path-exists-p path2))))

(def-ui-test make-file-test
  (let ((path (path-from-string "a.file" :fs fs)))
    (make-file path)
    (!t (path-exists-p path))))

(def-ui-test make-file-recursive-test
  (let ((path (path-from-string "/dir1/dir2/an.inner.file" :fs fs)))
    (make-file path :recursive t)
    (!t (path-exists-p path)))
  (let ((path (path-from-string "dir.a/dir.b/dir.c/an.another.inner.file" :fs fs)))
    (make-file path :recursive t)
    (!t (path-exists-p path))))

(def-ui-test make-file-in-non-existing-directory-error
  (let ((dir (path-from-string "dir/" :fs fs))
	(path (path-from-string "dir/file" :fs fs)))
    (!condition (make-file path) directory-does-not-exist-error
		(directory-does-not-exist-error-path dir))))

(def-ui-test make-directory-test
  (let ((path (path-from-string "a.directory/" :fs fs)))
    (make-directory path)
    (!t (path-exists-p path))))

(def-ui-test make-directory-recursive-test
  (let ((path (path-from-string "/dir1/dir2/an.inner.directory/" :fs fs)))
    (make-directory path :recursive t)
    (!t (path-exists-p path)))
  (let ((path (path-from-string "dir.a/dir.b/dir.c/inner.directory/" :fs fs)))
    (make-directory path :recursive t)
    (!t (path-exists-p path))))

(def-ui-test make-directory-in-non-existing-directory
  (let ((parent (path-from-string "outer.dir/" :fs fs))
	(path (path-from-string "outer.dir/inner.dir/" :fs fs)))
    (!condition (make-directory path) directory-does-not-exist-error
		(directory-does-not-exist-error-path parent))))

(def-ui-test correct-file-path-on-existing-file 
  (let ((path (path-from-string "a.file" :fs fs)))
    (make-file path)
    (!t (correct-path-p path))))

(def-ui-test correct-file-path-on-existing-directory-path
  (let ((path (path-from-string "a.file" :fs fs))
	(path-as-directory (path-from-string "a.file/" :fs fs)))
    (make-directory path-as-directory)
    (!not (correct-path-p path))))

(def-ui-test correct-directory-path-on-existing-directory
  (let ((path (path-from-string "a.dir/" :fs fs)))
    (make-directory path)
    (!t (correct-path-p path))))

(def-ui-test correct-directory-path-on-existing-file
  (let ((path (path-from-string "a.dir/" :fs fs))
	(path-as-file (path-from-string "a.dir" :fs fs)))
    (make-file path-as-file)
    (!not (correct-path-p path))))

(def-ui-test correct-path-p-in-existing-directory
  (let ((path (path-from-string "/home/a.file" :fs fs)))
    (!t (correct-path-p path))))

(def-ui-test correct-path-p-in-non-existing-directory
  (let ((path (path-from-string "/dir/file" :fs fs)))
    (!not (correct-path-p path))))

(defmacro path-check (name function result)
  `(!equalp (,function (path-from-string ,name :fs fs))
	    (path-from-string ,result :fs fs)))

(defmacro !parent= (name parent)
  `(path-check ,name parent-path ,parent))

(def-ui-test parent-path-for-file
  (!parent= "a.file" "")
  (!parent= "dir1/dir2/file.ext" "dir1/dir2/")
  (!parent= "/dir/file.ext" "/dir/"))

(def-ui-test parent-path-for-directory
  (!parent= "a.dir/" "")
  (!parent= "/a.dir/" "/")
  (!parent= "dir1/dir2/" "dir1/")
  (!parent= "/dir.a/dir.b/" "/dir.a/")
  (!null (parent-path (path-from-string "" :fs fs)))
  (!null (parent-path (path-from-string "/" :fs fs))))

(defmacro !as-file= (name file-name)
  `(path-check ,name path-as-file ,file-name))

(def-ui-test file-path-as-file
  (!as-file= "a.file" "a.file")
  (!as-file= "dir/file" "dir/file")
  (!as-file= "/dir/file" "/dir/file"))

(def-ui-test directory-path-as-file
  (!as-file= "dir/" "dir")
  (!as-file= "/dir/" "/dir")
  (let ((path (path-from-string "" :fs fs)))
    (!condition (path-as-file path) wrong-file-path-error
		(wrong-file-path-error-path path)))
  (let ((path (path-from-string "/" :fs fs)))
    (!condition (path-as-file path) wrong-file-path-error
		(wrong-file-path-error-path path))))


(defmacro !as-directory= (name directory-name)
  `(path-check ,name path-as-directory ,directory-name))

(def-ui-test file-path-as-directory
  (!as-directory= "file" "file/")
  (!as-directory= "/file" "/file/")
  (!as-directory= "dir/file" "dir/file/")
  (!as-directory= "/dir/file" "/dir/file/"))

(def-ui-test directory-path-as-directory
  (!as-directory= "dir/" "dir/")
  (!as-directory= "/dir/" "/dir/")
  (!as-directory= "dir1/dir2/" "dir1/dir2/")
  (!as-directory= "/dir1/dir2/" "/dir1/dir2/"))

(def-ui-test path-type-p-test
  (let ((paths (mapcar #'(lambda (x) (path-from-string x :fs fs)) '("file" "/file" "/dir/file" "dir/file"))))
    (!every #'file-path-p paths)
    (!every #'(lambda (x) (not (directory-path-p x))) paths))
  (let ((paths (mapcar #'(lambda (x) (path-from-string x :fs fs)) '("dir/" "/dir/" "/dir1/dir2/" "dir1/dir2/"))))
    (!every #'directory-path-p paths)
    (!every #'(lambda (x) (not (file-path-p x))) paths)))






	  