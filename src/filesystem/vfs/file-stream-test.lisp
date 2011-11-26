(in-package :burning-filesystem-test)

(defcase stream-test)

;;
;; Universal stream tests
;;

(deftest stream-test file-length-test ()
  (let ((fs (make-virtual-filesystem)))
    (let ((char-path (fs-path-from-string fs "char"))
	  (byte-path (fs-path-from-string fs "byte"))
	  (2byte-path (fs-path-from-string fs "2byte")))
      (let ((stream (fs-open-file fs char-path :direction :output :element-type 'character)))
	(write-string "abcdefgh" stream)
	(fs-close-stream fs stream))
      (!= (fs-file-length fs char-path 'character) 8)
      (let ((stream (fs-open-file fs byte-path :direction :output :element-type 'unsigned-byte)))
	(write-sequence '(100 200 50 100 10 20 1 2 3 4 5) stream)
	(fs-close-stream fs stream))
      (!= (fs-file-length fs byte-path 'unsigned-byte) 11)
      (let ((stream (fs-open-file fs 2byte-path :direction :output :element-type '(unsigned-byte 16))))
	(write-sequence '(10000 5000 20000 100 200 10 20 1 2 3 4 5) stream)
	(fs-close-stream fs stream))
      (!= (fs-file-length fs 2byte-path '(unsigned-byte 16)) 12))))

;;
;; Output stream tests
;;


(deftest stream-test simple-output-stream ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "test")))
    (let ((stream (fs-open-file fs path :direction :output)))
      (format stream "Hello, virtual file!!!~%")
      (fs-close-stream fs stream))
    (!equal (vfs-cat fs path) (format nil "Hello, virtual file!!!~%"))))

(defun test-stream (fs path test value &optional (element-type 'character))
  (let ((path (fs-path-from-string fs path)))
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
		   '(integer -10000 100000))
      (test-stream fs "test5"
		   #'(lambda (stream)
		       (write-sequence '(0 1 2 3 4 5) stream))
		   (result '(0 1 2 3 4 5))
		   'unsigned-byte)
      (test-stream fs "test6"
		   #'(lambda (stream)
		       (write-sequence '(1 2 3 4 5) stream :start 1 :end 3))
		   (result '(2 3))
		   'unsigned-byte))))
		       
(deftest stream-test output-stream-file-position ()
  (let ((fs (make-virtual-filesystem)))
    (let ((path (fs-path-from-string fs "char")))
      (let ((stream (fs-open-file fs path :direction :output :element-type 'character)))
	(!= (file-position stream) 0)
	(write-string "12345" stream)
	(!= (file-position stream) 5)
	(file-position stream 2)
	(!= (file-position stream) 2)
	(write-string "678" stream)
	(!= (file-position stream) 5)
	(fs-close-stream fs stream))
      (!equal (vfs-cat fs path) "12678"))
    (let ((path (fs-path-from-string fs "int")))
      (let ((stream (fs-open-file fs path :direction :output :element-type '(integer -10000 100000))))
	(!= (file-position stream) 0)
	(write-sequence '(-10000 2000) stream)
	(!= (file-position stream) 2)
	(file-position stream 1)
	(!= (file-position stream) 1)
	(write-sequence '(100000) stream)
	(!= (file-position stream) 2)
	(fs-close-stream fs stream))
      (!equal (vfs-cat fs path) 
	      (map 'string #'(lambda (x) (code-char x))
		   (list (ldb (byte 8 0) -10000)
			 (ldb (byte 8 8) -10000)
			 (ldb (byte 8 16) -10000)
			 (ldb (byte 8 0) 100000)
			 (ldb (byte 8 8) 100000)
			 (ldb (byte 8 16) 100000)))))))

(deftest stream-test writing-to-new-file ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "test")))
    (let ((stream (fs-open-file fs path :direction :output)))
      (write-string "some string" stream)
      (fs-close-stream fs stream))
    (!equal (vfs-cat fs path) "some string")))


(deftest stream-test writting-if-not-exists ()
  (let ((fs (make-virtual-filesystem)))
    (let ((path1 (fs-path-from-string fs "test1"))
	  (path2 (fs-path-from-string fs "test2"))
	  (path3 (fs-path-from-string fs "test3")))
      (!condition (fs-open-file fs path1 :direction :output :if-does-not-exist :error)
		  file-error (file-error-pathname path1))
      (fs-open-file fs path2 :direction :output :if-does-not-exist :create)
      (!t (fs-file-exists-p fs path2))
      (!null (fs-open-file fs path3 :direction :output :if-does-not-exist nil))
      (!condition (fs-open-file fs path1 :direction :output :if-does-not-exist :shut-down) error))))

(deftest stream-test output-element-type-test ()
  (let ((fs (make-virtual-filesystem)))
    (let ((stream (fs-open-file fs (fs-path-from-string fs "test-char") :direction :output)))
      (!equal (stream-element-type stream) 'character)
      (fs-close-stream fs stream))
    (let ((stream (fs-open-file fs (fs-path-from-string fs "test-byte") 
				:direction :output
				:element-type 'unsigned-byte)))
      (!equal (stream-element-type stream) '(unsigned-byte 8))
      (fs-close-stream fs stream))
    (let ((stream (fs-open-file fs (fs-path-from-string fs "test-int")
				:direction :output
				:element-type '(integer -12345 123456))))
      (!equal (stream-element-type stream) '(integer -12345 123456))
      (fs-close-stream fs stream))))

(deftest stream-test output-if-exists-default-test ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "file")))
    (fs-make-file fs path)
    (!condition (fs-open-file fs path :direction :output)
		file-error (file-error-pathname path))))

(deftest stream-test output-if-exists-rename-and-error-test ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "file"))
	 (bak-path (fs-path-from-string fs "file.bak")))
    (fs-make-file fs path)
    (!condition (fs-open-file fs path :direction :output :if-exists :error)
		file-error (file-error-pathname path))
    (!null (fs-open-file fs path :direction :output :if-exists nil))
    (flet ((check-rename (option)
	     (fs-close-stream fs (fs-open-file fs path :direction :output :if-exists option))
	     (!t (fs-file-exists-p fs path))
	     (!t (fs-file-exists-p fs bak-path))
	     (fs-delete-file fs bak-path)))
      (check-rename :new-version)
      (check-rename :rename))))

(deftest stream-test output-if-exists-delete-test ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "file")))
    (flet ((check-delete (option)
	     (let ((stream (fs-open-file fs path :direction :output)))
	       (write-string "12345" stream)
	       (fs-close-stream fs stream))
	     (fs-close-stream fs (fs-open-file fs path :direction :output :if-exists option))
	     (!= (fs-file-length fs path) 0)
	     (fs-delete-file fs path)))
      (check-delete :rename-and-delete)
      (check-delete :supersede))))

(deftest stream-test output-if-exists-overwrite-and-append-test ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "file")))
    (flet ((check-change (option result)
	     (let ((stream (fs-open-file fs path :direction :output)))
	       (write-string "12345" stream)
	       (fs-close-stream fs stream))
	     (let ((stream (fs-open-file fs path :direction :output :if-exists option)))
	       (write-string "67" stream)
	       (fs-close-stream fs stream))
	     (!equal (vfs-cat fs path) result)
	     (fs-delete-file fs path)))
      (check-change :overwrite "67345")
      (check-change :append "1234567"))))

(deftest stream-test rename-test ()
  (let* ((fs (make-virtual-filesystem))
	 (path (fs-path-from-string fs "/home/file.some.other.info.ext"))
	 (renamed-path (fs-path-from-string fs "/home/file.some.other.info.ext.bak"))
	 (twice-renamed-path (fs-path-from-string fs "/home/file.some.other.info.ext.bak.bak")))
    (flet ((touch-file (path)
	     (fs-close-stream fs (fs-open-file fs path :direction :output :if-exists :rename))))
	   (fs-make-file fs path)
	   (touch-file path)
	   (!t (fs-file-exists-p fs renamed-path))
	   (touch-file renamed-path)
	   (!t (fs-file-exists-p fs twice-renamed-path)))))

(deftest stream-test output-if-not-exists-defaults ()
  (let ((fs (make-virtual-filesystem)))
    (flet ((test-path (if-exists)
	     (let ((path (fs-path-from-string fs "test")))
	       (if (member if-exists '(:overwrite :append))
		   (!condition (fs-open-file fs path :direction :output :if-exists if-exists)
			       file-error (file-error-pathname path))
		   (progn (fs-close-stream fs (fs-open-file fs path :direction :output :if-exists if-exists))
			  (!t (fs-file-exists-p fs path))
			  (fs-delete-file fs path))))))
      (mapcar #'test-path '(:error :new-version :rename :rename-and-delete :overwrite :append :supersede nil)))))

(defun echo (fs path seq &optional (element-type 'character))
  (let ((stream (fs-open-file fs path :direction :output :element-type element-type)))
    (write-sequence seq stream)
    (fs-close-stream fs stream)))

(defmacro def-stream-test (name &body body)
  `(deftest stream-test ,name ()
     (let* ((fs (make-virtual-filesystem))
	    (path (fs-path-from-string fs "file")))
       ,@body)))

(def-stream-test simple-string-reading
  (echo fs path "I am virtual file.")
  (let ((stream (fs-open-file fs path :direction :input))
	(string (make-array 100 :element-type 'character)))
    (!= (read-sequence string stream) 18)
    (!equal (subseq string 0 18) "I am virtual file.")))

(def-stream-test character-reading
  (echo fs path (format nil "blablaCHARA one simple string~%Another simple string"))
  (let ((stream (fs-open-file fs path :direction :input))
	(string (make-array 100 :element-type 'character)))
    (!= (read-sequence string stream :start 0 :end 3) 3)
    (!equal (subseq string 0 3) "bla")
    (!= (read-sequence string stream :start 3 :end 6) 3)
    (!equal (subseq string 0 6) "blabla")
    (!eq (read-char stream) #\C)
    (!eq (read-char stream) #\H)
    (!eq (read-char stream) #\A)
    (!eq (read-char stream) #\R)
    (!equal (read-line stream) "A one simple string")
    (!equal (read-line stream) "Another simple string")))

(def-stream-test peeking-test
  (echo fs path "ab")
  (let ((stream (fs-open-file fs path :direction :input)))
    (flet ((test-char (char)
	     (dotimes (i 10)
	       (!eq (peek-char nil stream) char))
	     (!eq (read-char stream) char)))
      (test-char #\a)
      (test-char #\b))))

(def-stream-test char-eof-test
  (fs-make-file fs path)
  (let ((stream (fs-open-file fs path :direction :input)))
    (!null (read-char stream nil))))

(def-stream-test binary-reading
  (declare (ignore path))
  (let ((byte-path (fs-path-from-string fs "byte")))
    (echo fs byte-path '(100 200 50 100 10) 'unsigned-byte)
    (let ((stream (fs-open-file fs byte-path :direction :input :element-type 'unsigned-byte)))
      (!= (read-byte stream) 100)
      (let ((seq (make-array 5 :element-type 'unsigned-byte :initial-element 0)))
	(!= (read-sequence seq stream :start 1 :end 4) 3)
	(!equalp seq #(0 200 50 100 0)))
      (!= (read-byte stream) 10)))
  (let ((2byte-path (fs-path-from-string fs "2byte")))
    (echo fs 2byte-path '(1000 -2000 10000) '(signed-byte 16))
    (let ((stream (fs-open-file fs 2byte-path :direction :input :element-type '(signed-byte 16))))
      (!= (read-byte stream) 1000)
      (!= (read-byte stream) -2000)
      (!= (read-byte stream) 10000)))
  (let ((int-path (fs-path-from-string fs "int")))
    (echo fs int-path '(-10000 100000 -1000) '(integer -10000 100000))
    (let ((stream (fs-open-file fs int-path :direction :input :element-type '(integer -10000 100000))))
      (!= (read-byte stream) -10000)
      (!= (read-byte stream) 100000)
      (!= (read-byte stream) -1000))))

(def-stream-test binary-eof-test 
  (fs-make-file fs path)
  (!null (read-byte (fs-open-file fs path :direction :input :element-type 'signed-byte) nil)))

(def-stream-test input-file-position 
  (echo fs path "a simple file")
  (let ((stream (fs-open-file fs path :direction :input))
	(string (make-array 5 :initial-element #\Space :element-type 'character)))
    (!= (file-position stream) 0)
    (read-sequence string stream :end 5)
    (!= (file-position stream) 5)
    (file-position stream 2)
    (!= (file-position stream) 2)
    (read-sequence string stream)
    (!= (file-position stream) 7)
    (!equal string "simpl")
    (read-sequence string stream)
    (!= (file-position stream) 12)
    (!equal string "e fil")
    (read-sequence string stream)
    (!= (file-position stream) 13)
    (!equal string "e fil")))

(def-stream-test if-exist-ignoring 
  (fs-make-file fs path)
  (!condition-safe (fs-open-file fs path :direction :input :if-exists :error)))

(def-stream-test input-if-does-not-exists-defaults
  (!condition (fs-open-file fs path :direction :input) file-error
	      (file-error-pathname path)))

;;
;; io streams
;;

(def-stream-test io-character-test
  (echo fs path "char a string 1")
  (let ((stream (fs-open-file fs path :direction :io :if-exists :overwrite))
	(string (make-array 8 :element-type 'character :initial-element #\Space)))
    (read-sequence string stream :start 0 :end 4)
    (!equal string "char    ")
    (file-position stream 7)
    (read-sequence string stream :start 2 :end 8)
    (!equal string "chstring")
    (file-position stream 5)
    (write-char #\b stream)
    (file-position stream 14)
    (write-char #\2 stream)
    (fs-close-stream fs stream))
  (!equal (vfs-cat fs path) "char b string 2"))

(def-stream-test io-binary-test
  (echo fs path '(1 2 3 4 5) 'unsigned-byte)
  (let ((stream (fs-open-file fs path :direction :io :if-exists :overwrite :element-type 'unsigned-byte))
	(array (make-array 2)))
    (read-sequence array stream)
    (!equalp array #(1 2))
    (write-byte 10 stream)
    (read-sequence array stream)
    (!equalp array #(4 5))
    (fs-close-stream fs stream))
  (!equal (vfs-cat fs path) (map 'string #'code-char '(1 2 10 4 5))))

(def-stream-test io-if-exists-default
  (fs-make-file fs path)
  (!condition (fs-open-file fs path :direction :io) file-error 
	      (file-error-pathname path)))

(def-stream-test io-if-does-not-exist-default 
  (!condition-safe (fs-close-stream fs (fs-open-file fs path :direction :io)))
  (!t (fs-file-exists-p fs path)))

;;
;; probe streams
;;

(def-stream-test probe-stream-test
  (fs-make-file fs path)
  (!not (null (fs-open-file fs path :direction :probe)))
  (let ((new-path (fs-path-from-string fs "newpath")))
    (!null (fs-open-file fs new-path :direction :probe))))

(def-stream-test binary-probe-streams
  (fs-make-file fs path)
  (!not (null (fs-open-file fs path :direction :probe :element-type 'unsigned-byte))))

;;
;; General tests
;;

(def-stream-test default-direction-test
  (echo fs path "bla")
  (let ((stream (fs-open-file fs path))
	(string (make-array 3 :element-type 'character)))
    (read-sequence string stream)
    (!equal string "bla")
    (!condition (write-char #\e stream) error)))

(def-stream-test stream-closing-test
  (let ((stream (fs-open-file fs path :direction :output)))
    (write-sequence "blabla" stream)
    (fs-close-stream fs stream)
    (write-char #\e stream))
  (!equal (vfs-cat fs path) "blabla"))

(def-stream-test simple-write-locking 
  (fs-open-file fs path :direction :output)
  (!condition (fs-open-file fs path :direction :output :if-exists :overwrite) file-lock-error
	      (file-lock-error-path path)))

(def-stream-test closing-twice
  (flet ((open-path ()
	   (fs-open-file fs path :direction :output :if-exists :overwrite :if-does-not-exist :create)))
    (let ((stream (open-path)))
      (fs-close-stream fs stream)
      (open-path)
      (fs-close-stream fs stream)
      (!condition (open-path) file-lock-error))))

(def-stream-test deleting-locked-file
  (fs-open-file fs path :direction :input :if-does-not-exist :create)
  (!condition (fs-delete-file fs path) file-lock-error 
	      (file-lock-error-path path)))

(def-stream-test no-read-locking
  (fs-make-file fs path)
  (fs-open-file fs path :direction :input)
  (!condition-safe (fs-open-file fs path :direction :input)))

(def-stream-test reading-locking
  (fs-make-file fs path)
  (fs-open-file fs path :direction :input)
  (!condition (fs-open-file fs path :direction :output :if-exists :overwrite) file-lock-error))

(def-stream-test no-read-with-write
  (fs-open-file fs path :direction :output)
  (!condition (fs-open-file fs path :direction :input) file-lock-error))

(def-stream-test write-lock-releasing
  (fs-close-stream fs (fs-open-file fs path :direction :output))
  (!condition-safe (fs-open-file fs path :direction :input)))
    
(def-stream-test read-lock-releasing
  (let ((stream1 (fs-open-file fs path :direction :input :if-does-not-exist :create))
	(stream2 (fs-open-file fs path :direction :input :if-does-not-exist :create)))
    (fs-close-stream fs stream1)
    (fs-close-stream fs stream2)
    (!condition-safe (fs-close-stream fs (fs-open-file fs path :direction :input)))
    (!condition-safe (fs-close-stream fs (fs-open-file fs path :direction :output :if-exists :overwrite)))))


(def-stream-test probe-does-not-lock
  (fs-make-file fs path)
  (fs-open-file fs path :direction :probe)
  (!condition-safe (fs-close-stream fs (fs-open-file fs path :direction :input)))
  (!condition-safe (fs-close-stream fs (fs-open-file fs path :direction :output :if-exists :overwrite))))

(def-stream-test probing-locked-file
  (fs-open-file fs path :direction :output)
  (!not (null (fs-open-file fs path :direction :probe))))

