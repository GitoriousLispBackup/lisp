(in-package :burning-filesystem)

;;
;; Stream classes
;;

(defclass vfs-stream (trivial-gray-stream-mixin fundamental-stream)
  ((value :initarg :value :reader vfs-stream-value)
   (position :initarg :position :accessor vfs-stream-position)
   (length :initarg :length :accessor vfs-stream-length)
   (element-length :initarg :element-length :initform 1 :reader vfs-stream-element-length)))

(defclass vfs-input-stream (vfs-stream fundamental-input-stream) ())
(defclass vfs-output-stream (vfs-stream fundamental-output-stream) ())

(defclass vfs-binary-stream (vfs-stream fundamental-binary-stream)
  ((element-type :initarg :element-type :reader vfs-stream-element-type)))
(defclass vfs-character-stream (vfs-stream fundamental-character-stream) ())

(defclass vfs-binary-output-stream (vfs-binary-stream vfs-output-stream) ())
(defclass vfs-character-output-stream (vfs-output-stream vfs-character-stream) ())

(defclass vfs-binary-input-stream (vfs-binary-stream vfs-output-stream) ())
(defclass vfs-character-input-stream (vfs-input-stream vfs-character-stream) ())

(defclass vfs-character-io-stream (vfs-character-input-stream vfs-character-output-stream) ())

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

(defun character-type-p (type)
  (member type '(character base-char standard-char extended-char)))

(defun signed-type-p (type)
  (or (eq (first type) 'signed-byte)
      (and (eq (first type) 'integer) (< (second type) 0))))

(defun vfs-open-stream (direction value position length element-type)
  (flet ((character-stream-type (direction)
	   (ecase direction
	     (:input 'vfs-character-input-stream)
	     (:output 'vfs-character-output-stream)
	     (:io 'vfs-character-io-stream)
	     (:probe 'vfs-character-stream)))
	 (binary-stream-type (direction)
	   (ecase direction
	     (:input 'vfs-binary-input-stream)
	     (:output 'vfs-binary-output-stream)
	     (:io 'vfs-binary-io-stream)
	     (:probe 'vfs-binary-stream))))
    (if (character-type-p element-type)
	(make-instance (character-stream-type direction)
		       :value value
		       :position position
		       :length length)
	(let ((type (vfs-element-type element-type)))
	  (make-instance (binary-stream-type direction)
			 :value value
			 :position position
			 :length length
			 :element-type (first type)
			 :element-length (ceiling (second type) 8))))))

(def-vfs-method fs-open-stream (fs path direction element-type position)
  (let ((file (vfs-find-file fs path)))
    (let* ((value (vfsf-value file))
	   (length (length value))
	   (position (ecase position 
		       (:start 0)
		       (:end length))))
      (vfs-open-stream direction value position length element-type))))

(def-vfs-method fs-file-length (fs path &optional (element-type 'unsigned-byte))
  (let ((stream (fs-open-file fs path :element-type element-type)))
    (unwind-protect
	 (floor (/ (vfs-stream-length stream) (vfs-stream-element-length stream)))
      (fs-close-stream fs stream))))

(def-vfs-method fs-close-stream (fs stream)
  (declare (ignore stream))
  ())

;;
;; Generic streams
;;

(defmethod stream-element-type ((stream vfs-binary-stream))
  (vfs-stream-element-type stream))

(defmethod stream-file-position ((stream vfs-stream))
  (/ (vfs-stream-position stream) (vfs-stream-element-length stream)))

(defmethod (setf stream-file-position) (value (stream vfs-stream))
  (setf (vfs-stream-position stream) (* value (vfs-stream-element-length stream))))

;;
;; Output streams
;;

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

(defmethod stream-write-byte ((stream vfs-binary-output-stream) value)
  (dotimes (i (vfs-stream-element-length stream))
    (vfs-write-character stream (code-char (ldb (byte 8 (* i 8)) value)))))

;;
;; Input streams
;;

(defgeneric vfs-stream-read-element (stream))

(defmethod vfs-stream-read-element ((stream vfs-character-input-stream))
  (stream-read-char stream))

(defmethod vfs-stream-read-element ((stream vfs-binary-input-stream))
  (stream-read-byte stream))

(defun vfs-read-character (stream)
  (with-accessors ((value vfs-stream-value)
		   (position vfs-stream-position)
		   (length vfs-stream-length))
      stream
    (if (= position length)
	:eof
	(let ((value (aref value position)))
	  (incf position)
	  value))))

(defmethod stream-read-char ((stream vfs-character-input-stream))
  (vfs-read-character stream))

(defmethod stream-unread-char ((stream vfs-character-input-stream) char)
  (declare (ignore char))
  (when (= (vfs-stream-position stream) 0)
    (error "Cannot unread character in empty stream."))
  (decf (vfs-stream-position stream)))
  
(defmethod stream-read-byte ((stream vfs-binary-input-stream))
  (let ((value (do ((value 0)
		    (i 0 (incf i)))
		   ((or (eq value :eof) (= i (vfs-stream-element-length stream))) value)
		 (let ((read (vfs-read-character stream)))
		   (if (eq read :eof)
		       (setf value :eof)
		       (setf (ldb (byte 8 (* i 8)) value) (char-code read)))))))
    (if (and (numberp value)
	     (signed-type-p (stream-element-type stream))
	     (>= value (expt 2 (- (* 8 (vfs-stream-element-length stream)) 1))))
	(- value (expt 2 (* 8 (vfs-stream-element-length stream))))
	value)))

(defmethod stream-read-sequence (stream sequence start end &key)
  (let ((length (min (- (vfs-stream-length stream) (vfs-stream-position stream))
		     (- end start))))
    (dotimes (i length)
      (setf (elt sequence (+ start i)) (vfs-stream-read-element stream)))
    length))
  