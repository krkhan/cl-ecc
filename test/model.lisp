;;;; model.lisp

(in-package #:cl-ecc.test)


(defparameter *tests-list* '())
(defparameter *passed-tests* '())

(defmacro def-positive-test (funcname args &body body)
  (let ((ex (gensym)))
    `(progn
       (push ',funcname *tests-list*)
       (defmethod ,funcname ,args
         (handler-case
           (progn
             ,@body
             (format t "--- ~a passed~%~%" ',funcname)
             (push ',funcname *passed-tests*))
           (error (,ex)
             (format t "!!! ~a failed: caught unexpected error: ~a~%~%"
                     ',funcname ,ex)))))))

(defmacro def-negative-test (funcname expected-error args &body body)
  (let ((ex (gensym)))
    `(progn
       (push ',funcname *tests-list*)
       (defmethod ,funcname ,args
         (handler-case
           (progn
             ,@body
             (format t "!!! ~a failed: did not catch expected error~%~%"
                     ',funcname))
           (,expected-error (,ex)
             (format t "--- ~a passed -- caught expected error: ~a~%~%"
                     ',funcname ,ex)
             (push ',funcname *passed-tests*)))))))
