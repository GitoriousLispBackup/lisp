(in-package :burning-lexical)

;;
;; BUFFERS LIST
;;

(defun buffer-next (buffer)
  (third buffer))

(defun buffer-size (buffer)
  (second buffer))

(defun buffer-value (buffer)
  (first buffer))

(defun make-buffer (size stream)
  (let* ((value (make-array size :element-type 'character))
	 (size (read-sequence value stream)))
    (list value size nil)))

(defun read-buffer (buffer stream size)
  (setf (third buffer) (make-buffer size stream)))

;;
;;  SIMPLE ITERATOR
;;

(defun make-simple-iterator (buffers-list position)
  (cons buffers-list position))

(defun buffer-position (iterator)
  (the fixnum (rest iterator)))

(defun (setf buffer-position) (value iterator)
  (setf (rest iterator) value))

(defun buffers (iterator)
  (first iterator))

(defun (setf buffers) (value iterator)
  (setf (first iterator) value))

(defun eol-p (iterator)
  (declare (optimize (speed 3) (safety 0)))
  (= (the fixnum (buffer-position iterator))
     (the fixnum (1- (buffer-size (buffers iterator))))))

(defun copy-simple-iterator (iterator)
  (make-simple-iterator (buffers iterator) 
			(buffer-position iterator)))

(defun move-to-next-buffer (iterator)
  (setf (buffer-position iterator) -1)
  (setf (buffers iterator) (buffer-next (buffers iterator))))

(defun move-to-forward (forward back)
  (cond 
    ((eq (buffers forward)
	 (buffers back))
     (let ((value (subseq (buffer-value (buffers forward))
			  (1+ (buffer-position back))
			  (1+ (buffer-position forward)))))
       (setf (buffer-position back) (buffer-position forward))
       value))
    (t
     (let ((value (subseq (buffer-value (buffers back))
			  (1+ (buffer-position back)))))
       (move-to-next-buffer back)
       (concatenate 'string value (move-to-forward forward back))))))

;;
;; ITERATOR
;;

;;buffer 
;; * data
;; * max-size

;;iterator
;; * forward : simple iterator
;; * buffer size
;; * stream

(defun forward (iterator)
  (first iterator))

(defun back (iterator)
  (fourth iterator))

(defun make-iterator (stream &optional (buffer-size 4096))
  (let* ((buffers (make-buffer buffer-size stream))
	 (forward (make-simple-iterator buffers -1))
	 (back (make-simple-iterator buffers -1)))
    (list forward buffer-size stream back)))

(defun iterator-stream (iterator)
  (third iterator))

(defun iterator-buffer-size (iterator)
  (second iterator))

(defun try-read-buffer (iterator)
  (let ((forward (forward iterator)))
    (if (= (buffer-position forward) (1- (iterator-buffer-size iterator)))
	(progn
	  (when (null (buffer-next (buffers forward)))
	    (read-buffer (buffers forward) (iterator-stream iterator) (iterator-buffer-size iterator)))
	  (move-to-next-buffer forward)
	  (not (eof-p iterator)))
	nil)))

(defun eof-p (iterator)
  (declare (optimize (speed 3) (safety 0)))
  (let ((forward (forward iterator)))
    (if (eol-p forward)
        (not (try-read-buffer iterator))
	nil)))

(defmacro with-input-iterator ((iterator stream) &body body)
  `(let ((,iterator (make-iterator ,stream)))
     ,@body))

(defun get-next (iterator)
  (declare (optimize (speed 3) (safety 0)))
  (if (eof-p iterator)
      nil
      (aref (the (simple-array character *) (buffer-value (buffers (forward iterator))))
	    (incf (the fixnum (buffer-position (forward iterator)))))))

(defun reset (iterator)
  (setf (first iterator)
	(copy-simple-iterator (back iterator))))

(defun commit (iterator)
  (move-to-forward (forward iterator)
		   (back iterator)))
       
		 
	 
       