(in-package :burning-filesystem)

(defmacro def-vfs-method (name (fs &rest args) &body body)
  `(defmethod ,name ((,fs virtual-filesystem) ,@args)
     ,@body))

(defstruct (virtual-filesystem (:constructor %make-virtual-filesystem) (:conc-name vfs-))
  root
  current-path)

(defmethod print-object ((object virtual-filesystem) stream)
  (format stream "#S(VIRTUAL-FILESYSTEM :ROOT ~a :CURRENT-PATH ~a)" (vfs-root object) (vfs-current-path object)))

(defun vfs-current (fs)
  (vfs-find-directory fs (directory-path (vfs-current-path fs))))

(defstruct (vfs-directory (:conc-name vfsd-))
  parent
  directories
  files)

(defmethod print-object ((object vfs-directory) stream)
  (format stream "#S(VFS-DIRECTORY :DIRECTORIES ~a :FILES ~a)" (vfsd-directories object) (vfsd-files object)))

(defstruct (vfs-file (:conc-name vfsf-))
  (value (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))
  (readers 0)
  (write-lock-p nil))

(defun make-virtual-filesystem ()
  (let ((home-dir (make-vfs-directory))
	(work-dir (make-vfs-directory)))
    (let ((root (make-vfs-directory :directories (list (cons "home" home-dir) (cons "work" work-dir)))))
      (setf (vfsd-parent home-dir) root)
      (setf (vfsd-parent work-dir) root)
      (let ((fs (%make-virtual-filesystem :root root)))
	(setf (vfs-current-path fs) (fs-path-from-string fs "/work/"))
	fs))))

(defun divide (string char &key (include-middle nil) (from-end nil))
  (let ((pos (position char string :from-end from-end)))
    (if pos
	(list (subseq string 0 pos) (subseq string (if include-middle pos (1+ pos))))
	(list string))))

(defun path-to-list (string)
  (let ((division (divide string #\/)))
    (if (rest division)
	(cons (first division) (path-to-list (second division)))
	division)))

(defun parse-directory (list)
  (let ((directory (make-directory-path)))
    (labels 
	((parse-host (string)
	   (let ((parsed (divide string #\@)))
	     (when (rest parsed)
	       (setf (directory-host directory) (first parsed)))
	     (parse-device (first (last parsed)))))
	 (parse-device (string)
	   (let ((parsed (divide string #\:)))
	     (when (rest parsed)
	       (setf (directory-device directory) (first parsed)))
	     (parse-path (first (last parsed)))))
	 (?string (string)
	   (if string
	       (list string)
	       nil))
	 (parse-path (string)
	   (setf (directory-path directory) 
		 (cond 
		   ((equal string "") '(:absolute))
		   ((directory-host directory) `(:absolute ,@(?string string)))
		   (t `(:relative ,@(?string string)))))))
      (parse-host (first list))
      (setf (directory-path directory) (append (directory-path directory) (rest list)))
      directory)))

(defun parse-file (string)
  (labels 
      ((parse-type (string)
	 (let ((args (divide string #\. :from-end t)))
	   (if (not (eq (position #\. string) 0))
	       (make-file-path :name (first args) :type (second args))
	       (make-file-path :name (concatenate 'string "." (second args))))))
       (parse-version (string)
	 (let ((args (divide string #\:)))
	   (let ((file (parse-type (first args))))
	     (when (rest args)
	       (setf (file-version file) (second args)))
	     file))))
    (parse-version string)))

(def-vfs-method fs-path-from-string (fs string)
  (let ((string-list (path-to-list string)))
    (let ((directory (parse-directory (butlast string-list)))
	  (file (if (equal (first (last string-list)) "") 
		    nil 
		    (parse-file (first (last string-list))))))
      (if file
	  (progn
	    (setf (file-directory file) directory)
	    file)
	  directory))))

(def-vfs-method fs-path-to-string (fs (path directory-path))
  (with-output-to-string (stream)
    (when (directory-host path)
      (format stream "~a@" (directory-host path)))
    (when (directory-device path)
      (format stream "~a:" (directory-device path)))
    (when (eq (first (directory-path path)) :absolute)
      (format stream "/"))
    (dolist (i (rest (directory-path path)))
      (format stream "~a/" i))))

(defun filestring (path)
  (with-output-to-string (stream)
    (when (file-name path)
      (format stream "~a" (file-name path)))
    (when (file-type path)
      (format stream ".~a" (file-type path)))
    (when (file-version path)
      (format stream ":~a" (file-version path)))))

(def-vfs-method fs-path-to-string (fs (path file-path))
  (concatenate 'string
	       (fs-path-to-string fs (file-directory path))
	       (filestring path)))

(def-vfs-method fs-as-file-path (fs path)
  (let ((directory (make-directory-path :host (directory-host path)
					:device (directory-device path)
					:path (butlast (directory-path path))))
	(file (parse-file (first (last (directory-path path))))))
    (setf (file-directory file) directory)
    file))

(def-vfs-method fs-as-directory-path (fs path)
  (let ((directory (copy-directory-path (file-directory path))))
    (setf (directory-path directory) (append (directory-path directory) (list (filestring path))))
    directory))

(defun vfs-find-directory (fs path)
  (labels 
      ((do-find-subdirectory (dir name)
	 (cond 
	   ((string= name ".") dir)
	   ((string= name "..") (if (vfsd-parent dir) (vfsd-parent dir) dir))
	   (t (rest (assoc name (vfsd-directories dir) :test #'equal)))))
       (do-find-directory (dir path)
	 (cond 
	   ((null dir) nil)
	   ((null path) dir)
	   ((null (rest path)) (do-find-subdirectory dir (first path)))
	   (t (do-find-directory (do-find-subdirectory dir (first path)) (rest path))))))
    (cond 
      ((eq (first path) :absolute) (do-find-directory (vfs-root fs) (rest path)))
      ((eq (first path) :relative) (do-find-directory (vfs-current fs) (rest path)))
      (t (error "Wrong directory path specifier - ~a." path)))))

(defun vfs-find-file (fs path)
  (let ((directory (vfs-find-directory fs (directory-path (file-directory path)))))
    (rest (assoc (list (file-name path) (file-type path) (file-version path)) 
		 (vfsd-files directory)
		 :test #'equal))))

(def-vfs-method fs-list-directory (fs directory-path)
  (flet ((directory-name (dir)
	   (first (last (directory-path dir))))
	 (file-name (file)
	   (with-output-to-string (output)
	     (format output "~a" (file-name file))
	     (when (file-type file)
	       (format output ".~a" (file-type file)))
	     (when (file-version file)
	       (format output ":~a" (file-version file)))))
	 (subdir (entity)
	   (let ((subdir (copy-directory-path directory-path)))
	     (setf (directory-path subdir) (append (directory-path subdir) (list (first entity))))
	     subdir))
	 (subfile (entity)
	   (destructuring-bind (name type version) (first entity)
	     (make-file-path :directory (copy-directory-path directory-path) 
			     :name name 
			     :type type
			     :version version))))
    (let ((directory (vfs-find-directory fs (directory-path directory-path))))
      (nconc (sort (mapcar #'subdir (vfsd-directories directory)) #'string< :key #'directory-name)
	     (sort (mapcar #'subfile (vfsd-files directory)) #'string< :key #'file-name)))))

(def-vfs-method fs-make-directory (fs path)
  (let ((parent (vfs-find-directory fs (butlast (directory-path path)))))
    (push (cons (first (last (directory-path path))) (make-vfs-directory :parent parent))
	  (vfsd-directories parent))))

(defun find-file-directory (fs path)
  (vfs-find-directory fs (directory-path (file-directory path))))

(def-vfs-method fs-make-file (fs path)
  (let ((parent (find-file-directory fs path)))
    (push (cons (list (file-name path) (file-type path) (file-version path)) (make-vfs-file))
	  (vfsd-files parent))))

(def-vfs-method fs-current-directory (fs)
  (ui-path-path (make-ui-path :path (vfs-current-path fs) :filesystem fs)))

(def-vfs-method fs-home-directory (fs)
  (make-directory-path :host nil :device nil :path '(:absolute "home")))

(def-vfs-method fs-delete-directory (fs path)
  (let ((parent (vfs-find-directory fs (butlast (directory-path path)))))
    (setf (vfsd-directories parent) 
	  (remove (first (last (directory-path path))) (vfsd-directories parent) :test #'equal :key #'first))))

(def-vfs-method fs-delete-file (fs path)
  (let ((file (vfs-find-file fs path))
	(parent (find-file-directory fs path)))
    (when (or (vfsf-write-lock-p file) (> (vfsf-readers file) 0))
      (error 'file-lock-error :path path))
    (setf (vfsd-files parent)
	  (remove (list (file-name path) (file-type path) (file-version path)) (vfsd-files parent) 
		  :test #'equal :key #'first))))

(def-vfs-method fs-file-exists-p (fs path)
  (vfs-find-file fs path))

(def-vfs-method fs-directory-exists-p (fs path)
  (vfs-find-directory fs (directory-path path)))
    
(defun vfs-cat (fs path)
  (vfsf-value (vfs-find-file fs path)))

(def-vfs-method fs-file-replace (fs path action)
  (if (member action '(:new-version :rename))
      (let* ((directory (find-file-directory fs path))
	     (filename (first (assoc (list (file-name path) (file-type path) (file-version path)) 
				     (vfsd-files directory) :test #'equal))))
	(setf (first filename) (format nil "~a~:[~;~:*.~a~]" (first filename) (second filename)))
	(setf (second filename) "bak")
	(fs-make-file fs path))
      (call-next-method)))

(defun vfs-cd (ui-path)
  (unless (path-exists-p ui-path)
    (error "Cannot cd to non-existing path ~a." ui-path))
  (let ((ui-path (as-absolute-path ui-path)))
    (bind-ui-path (path fs) ui-path
      (setf (vfs-current-path fs) path))))


