(in-package :burning-filesystem-test)

(defcase stream-test)

(deftest stream-test simple-output-stream ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "test")))
    (fs-make-file fs path)
    (let ((stream (fs-open-file fs path :direction :output)))
      (format stream "Hello, virtual file!!!~%")
      (fs-close-stream fs stream))
    (!equal (vfs-cat fs path) (format nil "Hello, virtual file!!!~%"))))

(defun test-stream (fs path test value &optional (element-type 'character))
  (let ((path (fs-path-from-string fs path)))
    (fs-make-file fs path)
    (let ((stream (fs-open-file fs path :direction :output :element-type element-type)))
      (funcall test stream)
      (fs-close-stream fs stream))
    (!equal (vfs-cat fs path) value)))

(deftest stream-test char-write-functions ()
  (let ((fs (make-virtual-filesystem)))
    (test-stream fs "test1" 
		 #'(lambda (stream) (write-char #\B stream))
		 "B")
    (test-stream fs "test2" 
		 #'(lambda (stream) 
		     (write-string "bla-bla-bla " stream)
		     (write-string "prefixtextsuffix" stream :start 6 :end 10))
		 "bla-bla-bla text")
    (test-stream fs "test3"
		 #'(lambda (stream)
		     (write-line "bla-bla" stream))
		 (format nil "bla-bla~%"))))

(deftest stream-test binary-output-stream ()
  (let ((fs (make-virtual-filesystem)))
    (flet ((result (seq)
	     (map 'string #'(lambda (x) (code-char x)) seq)))
      (test-stream fs "test1"
		   #'(lambda (stream) 
		       (write-byte 100 stream)
		       (write-byte 200 stream))
		   (result '(100 200))
		   'unsigned-byte)
      (test-stream fs "test2"
		   #'(lambda (stream)
		       (write-byte -100 stream)
		       (write-byte 100 stream))
		   (result `(,(ldb (byte 8 0) -100) 100))
		   'signed-byte)
      (test-stream fs "test3"
		   #'(lambda (stream)
		       (write-byte 10000 stream)
		       (write-byte -10000 stream))
		   (result (list (ldb (byte 8 0) 10000) 
				 (ldb (byte 8 8) 10000) 
				 (ldb (byte 8 0) -10000)
				 (ldb (byte 8 8) -10000)))
		   '(signed-byte 16))
      (test-stream fs "test4"
		   #'(lambda (stream)
		       (write-byte 100000 stream)
		       (write-byte -10000 stream))
		   (result (list (ldb (byte 8 0) 100000)
				 (ldb (byte 8 8) 100000)
				 (ldb (byte 8 16) 100000)
				 (ldb (byte 8 0) -10000)
				 (ldb (byte 8 8) -10000)
				 (ldb (byte 8 16) -10000)))
		   '(integer -10000 100000)))))

(deftest stream-test writing-to-new-file ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "test")))
    (let ((stream (fs-open-file fs path :direction :output)))
      (write-string "some string" stream)
      (fs-close-stream fs stream))
    (!equal (vfs-cat fs path) "some string")))

;;test default if-not-exists
;;test if-not-exists

;;input streams
