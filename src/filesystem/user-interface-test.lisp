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

(def-ui-test make-directory-path-test 
  (declare (ignore fs))
  (let* ((base-directory (%make-up (make-directory-path :host 'a-host :device 'a-device :path 'a-path) fs))
	 (directory (copy-path base-directory)))
    (!eq (path-host directory) 'a-host)
    (!eq (path-device directory) 'a-device)
    (!eq (path-path directory) 'a-path))
  (let* ((default-directory (%make-up (make-dp 'default-host 'default-device 'default-path) fs))
	 (directory (copy-path default-directory :new-device 'my-device)))
    (!eq (path-host directory) 'default-host)
    (!eq (path-device directory) 'my-device)
    (!eq (path-path directory) 'default-path)))

(def-ui-test make-file-path-test
  (declare (ignore fs))
  (let ((directory (make-dp 'a-host 'a-device 'a-path)))
    (let* ((base-path (%make-up (make-fp directory 'name 'my-type 'the-best-choose) fs))
	   (path (copy-path base-path)))
      (!equalp (path-directory path) (%make-up directory fs))
      (!eq (path-name path) 'name)
      (!eq (path-type path) 'my-type)
      (!eq (path-version path) 'the-best-choose))
    (let* ((default-path (%make-up (make-fp directory 'default-name 'default-type 'default-version) fs))
	   (path (copy-path default-path :new-name 'my-name)))
      (!equalp (path-directory path) (%make-up directory fs))
      (!eq (path-name path) 'my-name)
      (!eq (path-type path) 'default-type)
      (!eq (path-version path) 'default-version))))

(defmacro !root= (name root-name)
  `(path-check ,name root-path ,root-name))

(def-ui-test root-path-test
  (!root= "a.file" "")
  (!root= "many/long/relative/directories/a.file" "")
  (!root= "/absolute.file" "/")
  (!root= "/many/long/absolute/directories/a.file" "/")
  (!root= "a.relative.directory/" "")
  (!root= "a/long/path/to/relative/directory/" "")
  (!root= "/an.absolute.directory/" "/")
  (!root= "/a/very/long/path/to/absolute/directory/" "/"))

(defmacro !path= (path1 path2)
  `(!equalp ,path1 ,path2))

(def-ui-test directory-concatenation
  (let ((r-dir1 (path-from-string "dir1/dir2/" :fs fs))
	(r-dir2 (path-from-string "dir3/dir4/" :fs fs))
	(a-dir1 (path-from-string "/a.dir/b.dir/" :fs fs))
	(a-dir2 (path-from-string "/c.dir/d.dir/" :fs fs)))
    (!path= (path+ r-dir1 r-dir2) (path-from-string "dir1/dir2/dir3/dir4/" :fs fs))
    (!path= (path+ a-dir1 r-dir1) (path-from-string "/a.dir/b.dir/dir1/dir2/" :fs fs))
    (!path= (path+ a-dir1 a-dir2) (path-from-string "/c.dir/d.dir/" :fs fs))
    (!path= (path+ r-dir1 a-dir2) (path-from-string "/c.dir/d.dir/" :fs fs))))

(def-ui-test directory-file-concatenation
  (let ((r-dir (path-from-string "dir1/dir2/" :fs fs))
	(a-dir (path-from-string "/dir1/dir2/" :fs fs))
	(r-file (path-from-string "dir/file" :fs fs))
	(a-file (path-from-string "/dir/file.ext" :fs fs)))
    (!path= (path+ r-dir r-file) (path-from-string "dir1/dir2/dir/file" :fs fs))
    (!path= (path+ r-dir a-file) (path-from-string "/dir/file.ext" :fs fs))
    (!path= (path+ a-dir r-file) (path-from-string "/dir1/dir2/dir/file" :fs fs))
    (!path= (path+ a-dir a-file) a-file)
    (!path= (path+ a-file a-file) a-file)
    (!path= (path+ a-file r-file) r-file)
    (!path= (path+ r-file a-file) a-file)
    (!path= (path+ r-file r-file) r-file)
    (!path= (path+ a-file r-dir) r-dir)
    (!path= (path+ r-file r-dir) r-dir)
    (!path= (path+ a-dir r-dir r-dir r-file) (path-from-string "/dir1/dir2/dir1/dir2/dir1/dir2/dir/file" :fs fs))))

(def-ui-test properties-concatenation 
  (let ((directory1 (%make-up (make-dp 'host1 'device1 '(:absolute "dir1" "dir2")) fs))
	(directory2 (%make-up (make-dp 'host2 'device2 '(:relative "dir3" "dir4")) fs)))
    (let ((file1 (copy-path directory1 :new-name 'name1 :new-type 'type1 :new-version 'version1))
	  (file2 (copy-path directory2 :new-name 'name2 :new-type 'type2 :new-version 'version2)))
      (!path= (path+ directory1 file2) 
	      (copy-path directory1 
			 :new-path '(:absolute "dir1" "dir2" "dir3" "dir4") 
			 :new-name 'name2
			 :new-type 'type2
			 :new-version 'version2))
      (!path= (path+ directory2 file1)
	      (copy-path file1 :new-device 'device2 :new-host 'host2))
      (!path= (path+ directory1 directory2)
	      (copy-path directory1 :new-path '(:absolute "dir1" "dir2" "dir3" "dir4")))
      (!path= (path+ directory2 directory1)
	      (copy-path directory2 :new-path (path-path directory1))))))

(def-ui-test delete-file-test
  (flet ((delete-test (name)
	   (let ((path (path-from-string name :fs fs)))
	     (make-file path)
	     (!t (path-exists-p path))
	     (remove-file path)
	     (!null (path-exists-p path)))))
    (delete-test "file.ext")
    (delete-test "/home/file.ext")
    (make-directory (path-from-string "dir/" :fs fs))
    (delete-test "dir/file.ext")))

(def-ui-test delete-file-fail-test
  (flet ((fail-test (name)
	   (let ((path (path-from-string name :fs fs)))
	     (!condition (remove-file path) file-does-not-exist-error
			 (file-does-not-exist-error-path path)))))
    (fail-test "file.ext")
    (fail-test "home")
    (fail-test "dir/file.ext")))

(def-ui-test delete-directory-test
  (flet ((delete-test (name)
	   (let ((path (path-from-string name :fs fs)))
	     (make-directory path)
	     (!t (path-exists-p path))
	     (remove-directory path)
	     (!null (path-exists-p path)))))
    (delete-test "a.dir/")
    (make-directory (path-from-string "dir/" :fs fs))
    (delete-test "dir/other.dir/")
    (delete-test "/work/a.sample.dir/")))
    
(def-ui-test delete-directory-with-files-fail-test
  (let ((path (path-from-string "dir/" :fs fs)))
    (make-directory path)
    (make-file (path-from-string "dir/a.file" :fs fs))
    (!condition (remove-directory path) directory-not-empty-error
		(directory-not-empty-error-path path))))

(def-ui-test delete-directory-with-subdirectories-fail-test
  (let ((path (path-from-string "dir/" :fs fs)))
    (make-directory path)
    (make-directory (path-from-string "dir/a.subdirectory/" :fs fs))
    (!condition (remove-directory path) directory-not-empty-error
		(directory-not-empty-error-path path))))

(def-ui-test delete-not-existing-directory-fail
  (let ((path (path-from-string "a.dir/" :fs fs)))
    (!condition (remove-directory path)	directory-does-not-exist-error
		(directory-does-not-exist-error-path path))))

(def-ui-test delete-directory-recursive-test
  (let ((path (path-from-string "dir/" :fs fs))
	(file-path (path-from-string "dir/file" :fs fs))
	(subdir-path (path-from-string "dir/subdir/" :fs fs))
	(subdir-file-path (path-from-string "dir/subdir/file" :fs fs)))
    (make-directory path)
    (make-file file-path)
    (make-directory subdir-path)
    (make-file subdir-file-path)
    (!condition-safe (remove-directory path t))))

;;recursive delete directory with subdirectories

;;list empty directory
;;list directory with files
;;list directory with files and subdirectories
;;list big directory recursive
;;list non-existing directory
