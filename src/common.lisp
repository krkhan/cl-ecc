;;;; common.lisp

(in-package #:cl-ecc.common)

(defmacro define-ecc-error (error-name)
  (let ((object (gensym))
        (out (gensym)))
    `(progn
       (define-condition ,error-name (error)
         ((msg :initarg :msg :reader error-msg)))
       (defmethod print-object ((,object ,error-name) ,out)
         (format ,out "~a" (error-msg ,object))))))

(defmacro validate-accessor-types (classname &rest typespeclist)
  (let ((i (gensym))
        (accessor (gensym))
        (typename (gensym)))
    `(defmethod initialize-instance :after ((object ,classname) &key)
       (do ((,i 0 (+ ,i 2))
            (,accessor)
            (,typename))
           ((>= ,i (length ',typespeclist)))
           (setf ,accessor (nth ,i ',typespeclist))
           (setf ,typename (nth (1+ ,i) ',typespeclist))
           (unless (typep (funcall ,accessor object) ,typename)
             (error 'invalid-type-error
                    :msg (format nil "~a must be of type ~a instead of ~a"
                                 ,accessor
                                 ,typename
                                 (type-of (funcall ,accessor object)))))))))

;; Errors

(define-ecc-error invalid-operation-error)
(define-ecc-error invalid-type-error)
