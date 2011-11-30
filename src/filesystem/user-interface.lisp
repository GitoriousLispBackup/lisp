(in-package #:burning-filesystem)

(define-condition wrong-filename-error (error) 
  ((string :initarg :string :reader wrong-filename-error-string)))

(defstruct ui-path 
  path
  filesystem)

(defmacro bind-ui-path ((path-symbol fs-symbol) ui-path &body body)
  (let ((ui-path-sym (gensym)))
    `(let ((,ui-path-sym ,ui-path))
       (let ((,path-symbol (ui-path-path ,ui-path-sym))
	     (,fs-symbol (ui-path-filesystem ,ui-path-sym)))
	 ,@body))))

;;
;; Path/string conversion
;;

(defun path-from-string (filename &key (type nil) (fs 'common-filesystem))
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
    (let ((file-p (or (file-path-p path) name-p type-p version-p)))
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

(defun remove-directory (path recursive-p)
  (bind-ui-path (path fs) path
    (fs-delete-directory fs path)))

;;
;; Working with files tree
;;

;  list-directory
;  resolve-path

;;
;; Standart directories
;;

;  home-directory
;  current-directory

;;
;; Working with streams
;;

;  open-file
;  close-stream
;  with-file



		
