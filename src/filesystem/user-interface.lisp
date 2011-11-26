(in-package #:burning-filesystem)

(define-condition wrong-filename-error (error) 
  ((string :initarg :string :reader wrong-filename-error-string)))

(defstruct ui-path 
  path
  filesystem)

(defun path-from-string (filename &key (type nil) (fs 'common-filesystem))
  (let ((path (fs-path-from-string fs filename)))
    (flet ((check-path (path)
	     (cond 
	       ((and (eq type :file) (directory-path-p path)) (error 'wrong-filename-error :string filename))
	       ((and (eq type :directory) (file-path-p path)) (fs-as-directory-path fs path))
	       (t path))))
      (make-ui-path :path (check-path path) :filesystem fs))))

(defun path-to-string (path)
  (fs-path-to-string (ui-path-filesystem path) (ui-path-path path)))
