;;;; common.lisp

(in-package #:cl-ecc)

(defun make-byte-array (size)
  (make-array size :element-type '(unsigned-byte 8)
                   :initial-element 0))


(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))
