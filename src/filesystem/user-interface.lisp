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

;  root-path

;  make-path
;  path+ 


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

;  relative-path-p
;  absolute-path-p

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

; delete-file
; delete-directory

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



		
