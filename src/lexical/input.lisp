(in-package :burning-lexical)

;;simple iterator
;; * buffer pointer
;; * position

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

(defun make-buffer (size stream)
  (let* ((value (make-array size :element-type 'character))
	 (size (read-sequence value stream)))
    (list value size)))

(defun make-iterator (stream &optional (buffer-size 4096))
  (let* ((buffers (list (make-buffer buffer-size stream)))
	 (forward (list buffers -1))
	 (back (list buffers -1)))
    (list forward buffer-size stream back)))

(defun read-buffer (list stream size)
  (setf (rest list) (list (make-buffer size stream))))

(defun iterator-stream (iterator)
  (third iterator))

(defun iterator-buffer-size (iterator)
  (second iterator))

(defun move-to-next-buffer (iterator)
  (setf (second iterator) -1)
  (setf (first iterator) (rest (first iterator))))

(defun try-read-buffer (iterator)
  (let ((forward (forward iterator)))
    (if (= (second forward) (1- (iterator-buffer-size iterator)))
	(progn
	  (when (null (cdar forward))
	    (read-buffer (first forward) (iterator-stream iterator) (iterator-buffer-size iterator)))
	  (move-to-next-buffer forward)
	  (not (eof-p iterator)))
	nil)))

(defun eof-p (iterator)
  (let ((forward (forward iterator)))
    (if (= (second forward)
	   (1- (cadar (first forward))))
	(not (try-read-buffer iterator))
	nil)))

(defmacro with-input-iterator ((iterator stream) &body body)
  `(let ((,iterator (make-iterator ,stream)))
     ,@body))

(defun get-next (iterator)
  (declare (optimize (speed 3) (safety 0)))
  (if (eof-p iterator)
      nil
      (progn
	(aref (caaar (forward iterator))
	      (incf (second (forward iterator)))))))

(defun reset (iterator)
  (setf (first iterator)
	(copy-list (back iterator))))

(defun commit (iterator)
  (cond 
    ((eq (first (forward iterator))
	 (first (back iterator)))
     (let ((value (subseq (caaar (forward iterator))
			  (1+ (second (back iterator)))
			  (1+ (second (forward iterator))))))
       (setf (second (back iterator)) (second (forward iterator)))
       value))
    (t 
     (let ((value (subseq (caaar (back iterator))
			  (1+ (second (back iterator))))))
       (move-to-next-buffer (back iterator))
       (concatenate 'string value (commit iterator))))))
       
		 
	 
       