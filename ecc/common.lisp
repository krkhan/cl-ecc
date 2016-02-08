;;;; common.lisp
(in-package #:cl-ecc)

;; Types
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype octet-vector (&optional length)
    (let ((length (or length '*)))
      `(simple-array (unsigned-byte 8) (,length)))))

;; Errors
(define-ecc-error invalid-operation-error)
(define-ecc-error invalid-type-error)
