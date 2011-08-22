(in-package :burning-lexical-test)

(defcase reglang-test)

(defun char-node (char)
  (range-node char char))

(defun no-enought-parameters-error (function-name)
  (let ((output (make-string-output-stream)))
    (format output "No enought parameters for operation ~a." (write-to-string function-name))
    (get-output-stream-string output)))

(defun too-much-parameters-error (function-name)
  (let ((output (make-string-output-stream)))
    (format output "Too much parameters for operation ~a." (write-to-string function-name))
    (get-output-stream-string output)))

(defun wrong-type-error (function-name position expected-type type)
  (let ((output (make-string-output-stream)))
    (format output 
	    "~a-th argument for ~a must be ~a, not ~a." 
	    position 
	    (write-to-string function-name)
	    expected-type
	    type)
    (get-output-stream-string output)))

(deftest reglang-test char-test ()
  (!node= (eval-regular '(:char #\a))
	  (char-node #\a))
  (!error (eval-regular '(:char))
	  (no-enought-parameters-error ':char))
  (!error (eval-regular '(:char #\a #\b))
	  (too-much-parameters-error ':char))
  (!error (eval-regular '(:char 1))
	  (wrong-type-error ':char 0 'character (type-of 1))))

(deftest reglang-test range-test ()
  (!node= (eval-regular '(:range #\a #\b))
	  (range-node #\a #\b))
  (!error (eval-regular '(:range #\a))
	  (no-enought-parameters-error ':range))
  (!error (eval-regular '(:range #\a #\b #\c))
	  (too-much-parameters-error ':range))
  (!error (eval-regular '(:range "bla-bla" #\a))
	  (wrong-type-error ':range 0 'character (type-of "bla-bla")))
  (!error (eval-regular '(:range #\a 0))
	  (wrong-type-error ':range 1 'character (type-of 0))))