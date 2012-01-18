(in-package #:burning-btr)

(defun configuration-path (path)
  (path+ path (path-from-string ".btr/repository.conf")))

(defun write-repository (repository path)
  (flet ((save-configuration (path)
	   (make-file path :recursive t)
	   (with-file (stream path :direction :output :if-exists :supersede)
	     (print-repository repository stream))))
    (save-configuration (configuration-path path))))

(defun open-repository (path)
  (with-file (stream (path+ path (path-from-string ".btr/repository.conf")))
    (read-repository stream)))

(defun repository-path (path)
  (cond
    ((file-path-p path) (repository-path (parent-path path)))
    ((path-exists-p (configuration-path path)) path)
    ((parent-path path) (repository-path (parent-path path)))
    ((relative-path-p path) 
     (let ((absolute-path (repository-path (as-absolute-path path))))
       (if absolute-path
	   (as-relative-path absolute-path path)
	   nil)))
    (t nil)))
