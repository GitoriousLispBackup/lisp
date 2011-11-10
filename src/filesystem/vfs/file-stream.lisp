(in-package :burning-filesystem)

(defclass vfs-stream (trivial-gray-stream-mixin fundamental-stream)
  ((value :initarg :value :reader vfs-stream-value)
   (position :initarg :position :accessor vfs-stream-position)
   (length :initarg :length :accessor vfs-stream-length)
   (element-length :initarg :element-length :initform 1 :reader vfs-stream-element-length)))

(defclass vfs-output-stream (vfs-stream fundamental-output-stream) ())

(defclass vfs-binary-stream (vfs-stream fundamental-binary-stream)
  ((element-type :initarg :element-type :reader vfs-stream-element-type)))

(defclass vfs-binary-output-stream (vfs-binary-stream vfs-output-stream) ())
(defclass vfs-character-output-stream (vfs-output-stream fundamental-character-output-stream) ())

(defun vfs-element-type (type)
  (cond
    ((eq type 'unsigned-byte) (list '(unsigned-byte 8) 8))
    ((eq type 'signed-byte) (list '(signed-byte 8) 8))
    ((eq (first type) 'integer) (list type
				      (1+ (max (integer-length (second type))
					       (integer-length (third type))))))
    ((or (eq (first type) 'signed-byte) 
	 (eq (first type) 'unsigned-byte)) (list type (second type)))
    (t (error "Unknown type specifier ~a." type))))

(def-vfs-method fs-open-output-stream (fs path &key element-type position)
  (let ((file (vfs-find-file fs path)))
    (let* ((value (vfsf-value file))
	   (length (length value))
	   (position (ecase position 
		       (:start 0)
		       (:end length))))
      (cond
	((eq element-type 'character) 
	 (make-instance 'vfs-character-output-stream
			:value value
			:position position
			:length length))
	(t (let ((type (vfs-element-type element-type)))
	     (make-instance 'vfs-binary-output-stream 
			    :value value
			    :position position
			    :length length
			    :element-type (first type)
			    :element-length (ceiling (second type) 8))))))))

(defmethod stream-element-type ((stream vfs-binary-stream))
  (vfs-stream-element-type stream))

(defun vfs-write-character (stream char)
  (with-accessors ((value vfs-stream-value) 
		   (position vfs-stream-position)
		   (length vfs-stream-length))
      stream
    (if (< position length)
	(progn (setf (aref value position) char)
	       (incf position))
	(progn (vector-push-extend char value)
	       (incf position)
	       (incf length)))))

(defgeneric vfs-stream-write-element (stream element))

(defmethod vfs-stream-write-element ((stream vfs-character-output-stream) element)
  (stream-write-char stream element))

(defmethod vfs-stream-write-element ((stream vfs-binary-output-stream) element)
  (stream-write-byte stream element))

(defmethod stream-write-char ((stream vfs-character-output-stream) char)
  (vfs-write-character stream char))

(defmethod stream-write-sequence ((stream vfs-output-stream) sequence start end &key)
  (dotimes (i (- end start))
    (vfs-stream-write-element stream (elt sequence (+ start i)))))

(def-vfs-method fs-close-stream (fs stream)
  (declare (ignore stream))
  ())

(defmethod stream-write-byte ((stream vfs-binary-output-stream) value)
  (dotimes (i (vfs-stream-element-length stream))
    (vfs-write-character stream (code-char (ldb (byte 8 (* i 8)) value)))))

(def-vfs-method fs-file-length (fs path &optional (element-type 'unsigned-byte))
  (let ((stream (fs-open-file fs path :direction :input :element-type element-type)))
    (unwind-protect
	 (floor (/ (vfs-stream-length stream) (vfs-stream-element-length stream)))
      (fs-close-stream fs stream))))
    
(defmethod stream-file-position ((stream vfs-output-stream))
  (/ (vfs-stream-position stream) (vfs-stream-element-length stream)))

(defmethod (setf stream-file-position) (value (stream vfs-output-stream))
  (setf (vfs-stream-position stream) (* value (vfs-stream-element-length stream))))