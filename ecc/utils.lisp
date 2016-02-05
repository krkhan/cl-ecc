;;;; utils.lisp
(in-package :cl-ecc)

;; Misc
(defmacro conc-to-symbol (&rest args)
  `(intern (concatenate 'string ,@(mapcar #'(lambda (x) `(string ,x))  args))))

;; Octet macros
(defmacro with-octets-to-integer ((&rest vars) &body body)
  `(let ,(mapcar #'(lambda (x) `(,x (ironclad::octets-to-integer ,x))) vars)
     (declare (type integer ,@vars))
     ,@body))
(defmacro with-octets-to-hex-string ((&rest vars) &body body)
  `(let ,(mapcar #'(lambda (x) `(,x (ironclad::byte-array-to-hex-string ,x))) vars)
     (declare (type integer ,@vars))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun octet-vector (length)
    (make-array length
                :element-type '(unsigned-byte 8)
                :initial-element 0)))

;; Error macros
(defmacro define-ecc-error (error-name)
  (let ((object (gensym))
        (out (gensym)))
    `(progn
       (define-condition ,error-name (error)
         ((msg :initarg :msg :reader error-msg)))
       (defmethod print-object ((,object ,error-name) ,out)
         (format ,out "~a" (error-msg ,object))))))
