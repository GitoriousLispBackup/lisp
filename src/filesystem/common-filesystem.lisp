(in-package :burning-filesystem)

;;
;; Pathnames
;;

(defun %component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun %directory-p (path)
  (and (not (%component-present-p (pathname-name path)))
       (not (%component-present-p (pathname-type path)))))

(defun %pathname-as-directory (pathname)
  (if (%directory-p pathname)
      pathname
      (make-pathname :directory (append (or (pathname-directory pathname) '(:relative))
					(list (file-namestring pathname)))
		     :name nil
		     :type nil
		     :defaults pathname)))

(defun %directory-wildcard (pathname)
  (make-pathname :name :wild
		 :type #-clisp :wild #+clisp nil
		 :defaults (%pathname-as-directory pathname)))

#+clisp
(defun %subdirectories-wildcard (pathname)
  (make-pathname :directory (append (pathname-directory pathname) '(:wild))
		 :name nil
		 :type nil
		 :defaults pathname))

(defun %file-exists-p (pathname)
  #+(or sbcl ccl)
  (probe-file pathname)
  #+clisp
  (or (ignore-errors (probe-file (%pathname-as-file pathname)))
      (ignore-errors
	(let ((directory-form (%pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))
  #-(or sbcl ccl clisp)
  (error "%file-exists-p not implemented."))

(defun %pathname-as-file (pathname)
  (if (%directory-p pathname)
      (let* ((directory (pathname-directory pathname))
	     (name-and-type (pathname (first (last directory)))))
	(make-pathname :directory (butlast directory)
		       :name (pathname-name name-and-type)
		       :type (pathname-type name-and-type)
		       :defaults pathname))
      pathname))

(defun %list-directory (path)
  (let ((wildcard (%directory-wildcard path)))
    #+sbcl
    (directory wildcard)
    #+ccl
    (directory wildcard :directories t)
    #+clisp
    (nconc (directory wildcard)
	   (directory (%subdirectories-wildcard wildcard)))
    #-(or sbcl ccl clisp)
    (error "%list-directory not implemented.")))

;;
;; Conversion between pathnames and fs structures.
;;

(defgeneric to-pathname (path))

(defmethod to-pathname ((directory directory-path))
  (make-pathname :name nil 
		 :type nil 
		 :version nil
		 :host (directory-host directory)
		 :device (directory-device directory)
		 :directory (directory-path directory)))

(defmethod to-pathname ((file file-path))
  (make-pathname :name (file-name file)
		 :type (file-type file)
		 :version (file-version file)
		 :defaults (to-pathname (file-directory file))))

(defun from-pathname (pathname)
  (flet ((do-make-directory-path (pathname)
	   (make-directory-path :host (pathname-host pathname)
				:device (pathname-device pathname)
				:path (pathname-directory pathname))))
  (if (%directory-p pathname)
      (do-make-directory-path pathname)
      (make-file-path :name (pathname-name pathname)
		      :type (pathname-type pathname)
		      :version (pathname-version pathname)
		      :directory (do-make-directory-path pathname)))))

;;
;; Implementation for fs-interface for common lisp pathnames.
;;

(defmethod fs-path-from-string ((fs (eql 'common-filesystem)) string)
  (from-pathname (pathname string)))

(defmethod fs-path-to-string ((fs (eql 'common-filesystem)) path)
  (namestring (to-pathname path)))

(defmethod fs-file-exists-p ((fs (eql 'common-filesystem)) file)
  (let ((found-path (%file-exists-p (to-pathname file))))
    (if (not found-path)
	nil
	(not (%directory-p found-path)))))

(defmethod fs-directory-exists-p ((fs (eql 'common-filesystem)) dir)
  (let ((found-path (%file-exists-p (to-pathname dir))))
    (if (not found-path)
        nil 
	(%directory-p found-path))))

(defmethod fs-list-directory ((fs (eql 'common-filesystem)) dir)
  (%list-directory (to-pathname dir)))

(defmethod fs-current-directory ((fs (eql 'common-filesystem)))
  #+clisp
  (ext:absolute-pathname "")
  #+sbcl
  (merge-pathnames "")
  #-(or clisp sbcl)
  (call-next-method))

(defmethod fs-home-directory ((fs (eql 'common-filesystem)))
  (user-homedir-pathname))

(defmethod fs-delete-file ((fs (eql 'common-filesystem)) file)
  (cl:delete-file (to-pathname file)))

(defmethod fs-delete-directory ((fs (eql 'common-filesystem)) dir)
  (let ((path (to-pathname dir)))
    #+clisp
    (ext:delete-directory path)
    #+sbcl
    (sb-ext:delete-directory path)
    #+ccl
    (ccl:delete-directory path)))

(defmethod fs-make-file ((fs (eql 'common-filesystem)) file)
  (open (to-pathname file) :direction :probe :if-does-not-exist :create))

(defmethod fs-make-directory ((fs (eql 'common-filesystem)) dir)
  (ensure-directories-exist (to-pathname dir)))

(defmethod fs-open-file ((fs (eql 'common-filesystem)) path 
			 &key (direction nil direction-p)
			 (element-type nil element-type-p)
			 (if-exists nil if-exists-p)
			 (if-does-not-exist nil if-does-not-exist-p))
  (apply #'open (to-pathname path)
	 (append (if direction-p `(:direction ,direction))
		 (if element-type-p `(:element-type ,element-type))
		 (if if-exists-p `(:if-exists ,if-exists))
		 (if if-does-not-exist-p `(:if-does-not-exist ,if-does-not-exist)))))

(defmethod fs-close-stream ((fs (eql 'common-filesystem)) stream)
  (close stream))

(defmethod fs-as-file-path ((fs (eql 'common-filesystem)) directory-path)
  (from-pathname (%pathname-as-file (to-pathname directory-path))))

(defmethod fs-as-directory-path ((fs (eql 'common-filesystem)) file-path)
  (from-pathname (%pathname-as-directory (to-pathname file-path))))

(defmethod fs-file-length ((fs (eql 'common-filesystem)) path &optional (element-type 'unsigned-byte))
  (with-open-file (stream (to-pathname path) :element-type element-type)
    (file-length stream)))