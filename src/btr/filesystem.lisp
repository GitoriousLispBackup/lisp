(in-package #:burning-btr)

(defun configuration-path (path)
  (path+ path (path-from-string ".btr/repository.conf")))

(defun write-repository (repository path)
  (flet ((save-configuration (path)
	   (make-file path :recursive t)
	   (with-file (stream path :direction :output :if-exists :supersede)
	     (print-repository repository stream))))
    (save-configuration (configuration-path path))))

(defun repository-path (path)
  (cond
    ((file-path-p path) (repository-path (parent-path path)))
    ((path-exists-p (configuration-path path)) path)
    ((parent-path path) (repository-path (parent-path path)))
    (t nil)))
