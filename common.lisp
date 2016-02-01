;;;; common.lisp

(in-package #:cl-ecc)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype octet-vector (&optional length)
    (let ((length (or length '*)))
      `(simple-array (unsigned-byte 8) (,length)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun octet-vector (length)
    (make-array length
                :element-type '(unsigned-byte 8)
                :initial-element 0)))
