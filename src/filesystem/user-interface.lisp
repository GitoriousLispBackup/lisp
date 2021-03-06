(in-package #:burning-filesystem)

(define-condition wrong-filename-error (error) 
  ((string :initarg :string :reader wrong-filename-error-string)))

(defstruct ui-path 
  path
  filesystem)

(defun path-filesystem (path)
  (ui-path-filesystem path))

(defmacro bind-ui-path ((path-symbol fs-symbol) ui-path &body body)
  (let ((ui-path-sym (gensym)))
    `(let ((,ui-path-sym ,ui-path))
       (let ((,path-symbol (ui-path-path ,ui-path-sym))
	     (,fs-symbol (ui-path-filesystem ,ui-path-sym)))
	 ,@body))))

;;
;; Path/string conversion
;;

(defvar *default-filesystem* 'common-filesystem)

(defun path-from-string (filename &key (type nil) (fs *default-filesystem*))
  (let ((path (fs-path-from-string fs filename)))
    (flet ((check-path (path)
	     (cond 
	       ((and (eq type :file) (%directory-path-p path)) (error 'wrong-filename-error :string filename))
	       ((and (eq type :directory) (%file-path-p path)) (fs-as-directory-path fs path))
	       (t path))))
      (make-ui-path :path (check-path path) :filesystem fs))))

(defun path-to-string (path)
  (fs-path-to-string (ui-path-filesystem path) (ui-path-path path)))

;;
;; Path utitlity functions
;;

(defun path= (path1 path2)
  (equalp path1 path2))

(defun copy-path (path &key 
		  (new-directory nil directory-p)
		  (new-host nil host-p)
		  (new-device nil device-p)
		  (new-path nil path-p)
		  (new-name nil name-p)
		  (new-type nil type-p)
		  (new-version nil version-p))
  (labels ((copy-directory (path)
	     (let ((directory (if directory-p (ui-path-path new-directory) (copy-directory-path path))))
	       (when host-p (setf (directory-host directory) new-host))
	       (when device-p (setf (directory-device directory) new-device))
	       (when path-p (setf (directory-path directory) new-path))
	       directory))
	   (copy-file (path)
	     (when (%directory-path-p path)
	       (setf path (make-file-path :directory path)))
	     (let ((file (copy-file-path path)))
	       (setf (file-directory file) (copy-directory (file-directory file)))
	       (when name-p (setf (file-name file) new-name))
	       (when type-p (setf (file-type file) new-type))
	       (when version-p (setf (file-version file) new-version))
	       file)))
    (let ((file-p (or (file-path-p path) new-name new-type new-version)))
      (bind-ui-path (path fs) path
	(if file-p
	    (make-ui-path :path (copy-file path) :filesystem fs)
	    (make-ui-path :path (copy-directory path) :filesystem fs))))))

(defmacro path-cond (var file-expr directory-expr)
  (let ((var-sym (gensym)))
    `(let ((,var-sym ,var))
       (cond
	 ((file-path-p ,var-sym) ,file-expr)
	 ((directory-path-p ,var-sym) ,directory-expr)))))

(defun path-host (path)
  (directory-host (ui-path-path (path-directory path))))

(defun path-device (path)
  (directory-device (ui-path-path (path-directory path))))

(defun path-path (path)
  (directory-path (ui-path-path (path-directory path))))

(defun path-directory (path)
  (path-cond path
	     (make-ui-path :path (file-directory (ui-path-path path)) :filesystem (ui-path-filesystem path))
	     path))

(defun path-name (path)
  (path-cond path
	     (file-name (ui-path-path path))
	     nil))

(defun path-type (path)
  (path-cond path
	     (file-type (ui-path-path path))
	     nil))

(defun path-version (path)
  (path-cond path
	     (file-version (ui-path-path path))
	     nil))

(defun %parent-directory (path)
  (let ((parent (copy-directory-path path)))
    (if (rest (directory-path parent))
	(progn (setf (directory-path parent) (butlast (directory-path path)))
	       parent)
	nil)))

(defun file-path-p (path)
  (%file-path-p (ui-path-path path)))

(defun directory-path-p (path)
  (%directory-path-p (ui-path-path path)))

(defun parent-path (path)
  (bind-ui-path (path fs) path
    (let ((path (cond ((%file-path-p path) (file-directory path))
		      ((%directory-path-p path) (%parent-directory path)))))
      (if path (make-ui-path :path path :filesystem fs) nil))))

(defun root-path (path)
  (let ((path (path-directory path)))
    (copy-path path :new-path (list (first (path-path path))))))

(define-condition wrong-file-path-error (error)
  ((path :initarg :path :reader wrong-file-path-error-path)))

(defun path-as-file (path)
  (if (file-path-p path)
      path
      (bind-ui-path (path fs) path
	(unless (rest (directory-path path))
	  (error 'wrong-file-path-error :path path))
	(make-ui-path :path (fs-as-file-path fs path) :filesystem fs))))

(defun path-as-directory (path)
  (if (directory-path-p path)
      path
      (bind-ui-path (path fs) path
	(make-ui-path :path (fs-as-directory-path fs path) :filesystem fs))))

(defun path+ (&rest paths)
  (labels ((merge-paths (path1 path2)
	     (cond
	       ((eq (first path2) :absolute) path2)
	       (t (append path1 (rest path2)))))
	   (directory+ (dir1 dir2)
	     (copy-path dir1 :new-path (merge-paths (path-path dir1) (path-path dir2))))
	   (file+ (path1 path2)
	     (if (file-path-p path1)
		 path2
		 (copy-path path2 :new-directory (directory+ (path-directory path1) (path-directory path2))))))
    (reduce #'file+ paths)))

(defun path- (path1 path2)
  (labels ((path-diff (path1 path2)
	     (cond 
	       ((null path2) (cons :relative path1))
	       ((null path1) nil)
	       ((equal (first path1) (first path2)) (path-diff (rest path1) (rest path2)))
	       (t nil))))
    (if (path= (root-path path1) (root-path path2))
	(let ((diff (path-diff (path-path path1) (path-path path2))))
	  (if diff
	      (copy-path path1
			 :new-host nil
			 :new-device nil
			 :new-path diff
			 :new-name (path-name path1)
			 :new-type (path-type path1)
			 :new-version (path-version path1))
	      nil))
	nil)))

;;
;; Checking paths
;;

(defun path-exists-p (path)
  (let ((path (ui-path-path path))
	(fs (ui-path-filesystem path)))
    (labels ((directory-exists-p (path)
	       (cond
		 ((null (rest (directory-path path))) t)
		 (t (and (directory-exists-p (%parent-directory path)) 
			 (fs-directory-exists-p fs path))))))
      (cond
	((%file-path-p path) (and (directory-exists-p (file-directory path))
				 (fs-file-exists-p fs path)))
	((%directory-path-p path) (directory-exists-p path))))))

(defun correct-path-p (path)
  (cond
    ((path-exists-p path) t)
    ((path-exists-p (path-as-file path)) nil)
    ((path-exists-p (path-as-directory path)) nil)
    (t (path-exists-p (parent-path path)))))

(defun relative-path-p (path)
  (ecase (first (path-path path))
    (:relative t)
    (:absolute nil)))

(defun absolute-path-p (path)
  (ecase (first (path-path path))
    (:relative nil)
    (:absolute t)))

(defun as-absolute-path (path)
  (labels ((%absolute-path (path dir)
	     (cond
	       ((string= dir ".") path)
	       ((string= dir "..") (butlast path))
	       (t (append path (list dir)))))
	   (%as-absolute-directory (path) 
	     (let ((parent (parent-path path)))
	       (if parent 
		   (copy-path path 
			      :new-path (%absolute-path (path-path (as-absolute-path parent))
							(first (last (path-path path)))))
		   path)))
	   (%as-absolute-path (path)
	     (path-cond path 
			(copy-path path :new-directory (as-absolute-path (path-directory path)))
			(%as-absolute-directory path))))
    (if (relative-path-p path)
	(as-absolute-path (path+ (current-directory (ui-path-filesystem path)) path))
	(%as-absolute-path path))))
	   
(defun as-relative-path (path base)
  (labels ((way-up (list)
	     (mapcar #'(lambda (x) (declare (ignore x)) "..") 
		     list))
	   (find-way (list1 list2)
	     (cond 
	       ((null list1) (way-up list2))
	       ((null list2) list1)
	       ((equal (first list1) (first list2)) (find-way (rest list1) (rest list2)))
	       (t (append (way-up list2) list1)))))
    (when (and (relative-path-p path) (absolute-path-p base))
      (setf path (as-absolute-path path)))
    (when (and (absolute-path-p path) (relative-path-p base))
      (setf base (as-absolute-path base)))
    (copy-path path :new-path (cons :relative (find-way (path-path path) (path-path base))))))

;;
;; Making and deleting filesystem objects
;;

(define-condition directory-does-not-exist-error (error) 
  ((path :initarg :path :reader directory-does-not-exist-error-path)))

(defun %ensure-directory-exists (path create-if-not)
  (unless (path-exists-p path)
    (if create-if-not
	(make-directory path :recursive create-if-not)
	(error 'directory-does-not-exist-error :path path)))
  t)

(defun make-file (path &key recursive)
  (%ensure-directory-exists (parent-path path) recursive)
  (bind-ui-path (path fs) path
    (fs-make-file fs path)))

(defun make-directory (path &key recursive)
  (%ensure-directory-exists (parent-path path) recursive)
  (bind-ui-path (path fs) path
    (fs-make-directory fs path)))

(define-condition file-does-not-exist-error (error) 
  ((path :initarg :path :reader file-does-not-exist-error-path)))

(defun remove-file (path)
  (unless (path-exists-p path)
    (error 'file-does-not-exist-error :path path))
  (bind-ui-path (path fs) path
    (fs-delete-file fs path)))

(define-condition directory-not-empty-error (error)
  ((path :initarg :path :reader directory-not-empty-error-path)))

(defun remove-directory (path &optional (recursive-p nil))
  (flet ((remove-path (path)
	   (path-cond path
		      (remove-file path)
		      (remove-directory path t))))
    (unless (path-exists-p path)
      (error 'directory-does-not-exist-error :path path))
    (when (list-directory path)
      (if recursive-p 
	  (mapc #'remove-path (list-directory path))
	  (error 'directory-not-empty-error :path path)))
    (bind-ui-path (path fs) path
      (fs-delete-directory fs path))))

;;
;; Working with files tree
;;

(defun list-directory (path &optional (recursive-p nil))
  (unless (path-exists-p path)
    (error 'directory-does-not-exist-error :path path))
  (labels ((do-list-directory (path)
	     (bind-ui-path (path fs) path
	       (mapcar #'(lambda (x) 
			   (make-ui-path :path x :filesystem fs)) 
		       (fs-list-directory fs path))))
	   (filter-files (list)
	     (remove-if-not #'file-path-p list))
	   (list-subdirectories (list)
	     (apply #'append (mapcar #'(lambda (dir) (list-directory dir t)) 
				     (remove-if-not #'directory-path-p list)))))
    (if recursive-p 
	(let ((dir-list (do-list-directory path)))
	  (append (filter-files dir-list) (list-subdirectories dir-list)))
	(do-list-directory path))))

(defun match-pattern (pattern string &optional (p-index 0) (s-index 0))
  (let ((pattern-empty-p (= p-index (length pattern)))
	(string-empty-p (= s-index (length string))))
    (cond
      ((and pattern-empty-p string-empty-p) t)
      (pattern-empty-p nil)
      (string-empty-p (and (char= (char pattern p-index) #\*) (match-pattern pattern string (1+ p-index) s-index)))
      ((char= (char pattern p-index) #\*) (or (match-pattern pattern string (1+ p-index) (1+ s-index))
					      (match-pattern pattern string p-index (1+ s-index))))
      ((char= (char pattern p-index) #\?) (match-pattern pattern string (1+ p-index) (1+ s-index)))
      (t (and (char= (char pattern p-index) (char string s-index)) 
	      (match-pattern pattern string (1+ p-index) (1+ s-index)))))))

(defun %resolve-file-path (path)
  (labels ((filestring (path)
	     (format nil "~a.~a" (path-name path) (path-type path)))
	   (do-resolve (dir path)
	     (let ((files (remove-if-not #'file-path-p (list-directory dir))))
	       (remove-if-not #'(lambda (file) (match-pattern (filestring path) (filestring file))) files))))
    (apply #'append (mapcar #'(lambda (dir) (do-resolve dir path)) 
			    (resolve-path (parent-path path))))))

(defun %resolve-directory-path (path)
  (labels ((name (path)
	     (first (last (path-path path))))
	   (do-resolve (dir path)
	     (let ((dirs (remove-if-not #'directory-path-p (list-directory dir))))
	       (remove-if-not #'(lambda (dir) (match-pattern (name path) (name dir))) dirs))))
    (let ((parent (parent-path path)))
      (if parent
	  (apply #'append (mapcar #'(lambda (dir) (do-resolve dir path))
				  (resolve-path (parent-path path))))
	  (if (path-exists-p path) (list path) nil)))))

(defun resolve-path (path)
  (path-cond path
	     (%resolve-file-path (as-absolute-path path))
	     (%resolve-directory-path (as-absolute-path path))))

;;
;; Standart directories
;;

(defun home-directory (&optional (fs *default-filesystem*))
  (make-ui-path :path (fs-home-directory fs) :filesystem fs))

(defun current-directory (&optional (fs *default-filesystem*))
  (make-ui-path :path (fs-current-directory fs) :filesystem fs))

;;
;; Working with streams
;;

(defun open-file (path &key 
		  (direction nil direction-p) 
		  (element-type nil element-p) 
		  (if-exists nil if-exists-p)
		  (if-does-not-exist nil if-does-not-exist-p))
  (bind-ui-path (path fs) path
    (apply #'fs-open-file fs path 
	   (append (if direction-p `(:direction ,direction))
		   (if element-p `(:element-type ,element-type))
		   (if if-exists-p `(:if-exists ,if-exists))
		   (if if-does-not-exist-p `(:if-does-not-exist ,if-does-not-exist))))))

(defun close-stream (stream &optional (fs *default-filesystem*))
  (fs-close-stream fs stream))

(defun length-of-file (path)
  (bind-ui-path (path fs) path
    (fs-file-length fs path)))

(defmacro with-file ((stream path &key 
			     (direction nil direction-p) 
			     (element-type nil element-p)
			     (if-exists nil if-exists-p)
			     (if-does-not-exist nil if-does-not-exist-p)) 
		     &body body)
  (let ((path-sym (gensym)))
    `(let ((,path-sym ,path))
       (let ((,stream (open-file ,path-sym
				 ,@(if direction-p `(:direction ,direction))
				 ,@(if element-p `(:element-type ,element-type))
				 ,@(if if-exists-p `(:if-exists ,if-exists))
				 ,@(if if-does-not-exist-p `(:if-does-not-exist ,if-does-not-exist)))))
	 (unwind-protect (progn ,@body)
	   (close-stream ,stream (ui-path-filesystem ,path-sym)))))))

(defun read-file (path)
  (with-file (stream path)
    (let ((string (make-string (length-of-file path))))
      (read-sequence string stream)
      string)))

(defun write-file (path string)
  (with-file (stream path :direction :output :if-exists :supersede)
    (write-sequence string stream)))


		
