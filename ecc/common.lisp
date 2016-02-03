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


;; Model helper macros

(defmacro define-generic-reader-functions (class)

  `(progn

     (defmethod get-slot :before (slot (object ,class) spec)
                (cond
                  ((and (eq slot 'g)
                        (typep object 'Curve)) (assert (typep (slot-value object slot) 'Point)))
                  (t (assert (typep (slot-value object slot) 'octet-vector)))))

     (defmethod get-slot ((spec (eql :int)) slot (object ,class) )
       (ironclad:octets-to-integer (slot-value object slot)))

     (defmethod get-slot ((spec (eql :vector)) slot (object ,class) )
       (slot-value object slot))

     (defmethod get-slot ((spec (eql :hex-string)) slot (object ,class))
       (ironclad:byte-array-to-hex-string (slot-value object slot)))

     (defmethod get-slot ((spec (eql :point)) slot (object ,class))
       (slot-value object slot))))

;; Instance macros

(defmacro define-slot-type-parser (classname &rest typespeclist)
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
                  (when (typep (slot-value object ,accessor) '(simple-array character))
                    (setf (slot-value object ,accessor) (ironclad:hex-string-to-byte-array (slot-value object ,accessor))))
                  (when (typep (slot-value object ,accessor) 'integer)
                    (setf (slot-value object ,accessor) (ironclad:integer-to-octets (slot-value object ,accessor))))))))

(defmacro define-slot-type-validator (classname &rest typespeclist)
  (let ((i (gensym))
        (accessor (gensym))
        (typename (gensym)))
    `(defmethod initialize-instance :around ((object ,classname) &key)
                (let ((instance (call-next-method)))
                  (do ((,i 0 (+ ,i 2))
                       (,accessor)
                       (,typename))
                      ((>= ,i (length ',typespeclist)) instance)
                    (setf ,accessor (nth ,i ',typespeclist))
                    (setf ,typename (nth (1+ ,i) ',typespeclist))
                    (unless (typep (slot-value object ,accessor) ,typename)
                      (error 'invalid-type-error
                             :msg (format nil "~a must be of type ~a instead of ~a.
                                               The program will parse integers and hex-strings to correct type."
                                          ,accessor ,typename (type-of (slot-value object ,accessor)))))
                    instance)))))



;; Errors

(defmacro define-ecc-error (error-name)
  (let ((object (gensym))
        (out (gensym)))
    `(progn
       (define-condition ,error-name (error)
         ((msg :initarg :msg :reader error-msg)))
       (defmethod print-object ((,object ,error-name) ,out)
         (format ,out "~a" (error-msg ,object))))))


(define-ecc-error invalid-operation-error)
(define-ecc-error invalid-type-error)
