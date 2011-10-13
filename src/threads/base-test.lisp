(in-package :burning-threads-test)

(defcase base-test)

(defun range (first last &key (accending t))
  (cond
    ((= first last) (list first))
    (accending (cons first (range (1+ first) last :accending t)))
    (t (cons last (range first (1- last) :accending nil)))))

(deftest base-test thread-creating-test ()
  (let ((threaded-queue ()))
    (let ((thread (make-thread #'(lambda () (push 'thread threaded-queue)))))
      (wait-thread thread)
      (!equal threaded-queue '(thread))))
  (let ((result-queue ())
	(threads ()))
    (dotimes (i 8)
      (push (spawn-thread #'(lambda (x) (dotimes (i 4) (push (+ (* 4 x) i) result-queue))) i)
	    threads))
    (apply #'wait-threads threads)
    (!equal result-queue (range 0 31 :accending nil))))

(deftest base-test threads-with-sleep-test ()
  (let ((result-queue ()))
    (let* ((thread1 (spawn-thread #'(lambda () (sleep 0.01) (push 0 result-queue))))
	   (thread2 (spawn-thread #'(lambda () (push 1 result-queue)))))
      (wait-threads thread1 thread2)
      (!equal result-queue '(0 1)))))
    