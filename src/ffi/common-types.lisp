(in-package :burning-ffi)

(defvar *ffi-types* ())

(defun %add-type (external-value internal-value)
  (if (not (equal (%get-type external-value) internal-value))
      (pushnew `(,external-value . ,internal-value) *ffi-types*)))

(defun %get-type (external-value)
  (rest (assoc external-value *ffi-types* :test #'equal)))

(defmacro deffitype (external-name internal-name)
  `(%add-type ,external-name ,internal-name))

(defgeneric get-type (name &rest args))

(defmethod get-type (name &rest args)
  (declare (ignore args))
  (%get-type name))

(deffitype :void :void)
(deffitype :bool :boolean)
(deffitype :int :int)
(deffitype :short :short)
(deffitype :long :long)
(deffitype :uint :uint)
(deffitype :ushort :ushort)
(deffitype :ulong :ulong)
(deffitype :float :float)
(deffitype :double :double)
(deffitype :string :string)