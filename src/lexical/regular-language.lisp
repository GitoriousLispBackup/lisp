(in-package :burning-lexical)

;Parses expression in regular language with specified operation and it's arguments
(defgeneric parse-regular (operation args))

(define-condition wrong-arguments-count (error) 
  ((function :initarg :function :reader wrong-function)
   (real-arguments-count :initarg :real-arguments-count :reader real-arguments-count)
   (arguments-count :initarg :arguments-count :reader arguments-count)))

(defmethod parse-regular (operation args)
  (error "Wrong operation ~a" operation))

(defun check-arguments-count (args count function-name)
  (let ((real-count (length args)))
    (cond
      ((< real-count count) (error "No enought parameters for operation ~a." function-name))
      ((> real-count count) (error "Too much parameters for operation ~a." function-name)))))

(defun check-argument-type (argument type function-name position)
  (unless (typep argument type)
    (error "~a-th argument for ~a must be ~a, not ~a." position function-name type (type-of argument))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun first-atom (item)
    (cond
      ((atom item) item)
      (t (first item)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-check (item position function-name)
    (cond
      ((atom item) nil)
      ((null (rest item)) nil)
      (t `(check-argument-type ,(first item) ,(second item) ,function-name ,position)))))

(defmacro defoperation (name args &body body)
  (let ((args-list ())
	(checks-list ())
	(args-name (gensym)))
    (dotimes (i (length args))
      (let ((item (nth i args)))
	(push `(,(first-atom item) (nth ,i ,args-name)) args-list)
	(push (type-check item i (write-to-string name)) checks-list)))
    `(defmethod parse-regular ((operation (eql ',name)) ,args-name)
       (check-arguments-count ,args-name ,(length args) ,(write-to-string name))
       (let ,args-list
	 ,@checks-list
	 ,@body))))

(defoperation :char ((char 'character)) 
  (character-node char))

(defoperation :range ((first 'character) (last 'character))
  (range-node first last))

(defun eval-regular (expr)
  (cond 
    ((not (listp expr)) (error "Regular expression must be a list. ~a" expr))
    (t (parse-regular (first expr) (rest expr)))))
