(in-package :burning-filesystem)

(defmacro def-vfs-method (name (fs &rest args) &body body)
  `(defmethod ,name ((,fs (eql 'vfs)) ,@args)
     ,@body))

(defun make-virtual-filesystem ()
  'vfs)

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

(def-vfs-method fs-path-to-string (fs (path file-path))
  (flet ((print-file (path)
	   (with-output-to-string (stream)
	     (when (file-name path)
	       (format stream "~a" (file-name path)))
	     (when (file-type path)
	       (format stream ".~a" (file-type path)))
	     (when (file-version path)
	       (format stream ":~a" (file-version path))))))
    (concatenate 'string
		 (fs-path-to-string fs (file-directory path))
		 (print-file path))))
	       
