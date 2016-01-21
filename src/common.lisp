;;;; common.lisp

(in-package #:cl-ecc)

(defmacro define-ecc-error (error-name)
  (let ((object (gensym))
        (out (gensym)))
    `(progn
       (define-condition ,error-name (error)
         ((msg :initarg :msg :reader error-msg)))
       (defmethod print-object ((,object ,error-name) ,out)
         (format ,out "~a" (error-msg ,object))))))


;; Errors

(define-ecc-error invalid-operation-error)
(define-ecc-error invalid-type-error)
