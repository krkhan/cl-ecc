;;;; common.lisp

(in-package #:cl-ecc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype octet-vector (&optional length)
    (let ((length (or length '*)))
      `(simple-array (unsigned-byte 8) (,length)))))

;;;; Types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun octet-vector (length)
    (make-array length
                :element-type '(unsigned-byte 8)
                :initial-element 0)))


;;;; Main

;; Reader function macros
(defmacro define-custom-octet-reader-functions (class &rest slots)
  `(progn
     ,@ (mapcar #'(lambda (slot) (generate-reader-functions class slot)) slots)))

(defmacro define-custom-composite-octet-reader-functions (class &rest args)
               `(progn
                  ,@ (mapcar #'(lambda (fn-def)
                                 (let ((fn (first fn-def))
                                       (fn-slots (rest fn-def)))
                                   (mapcan #'(lambda (slots)
                                               (generate-composite-reader-functions class fn slots)) (list fn-slots)))) args)))


;; Parser macros
(defmacro define-slot-to-octet-vector-parser (classname &rest typespeclist)
  (let ((i (gensym))
        (accessor (gensym)))
    `(defmethod initialize-instance :after ((object ,classname) &key)
                (do ((,i 0 (+ ,i 1))
                     (,accessor))
                    ((>= ,i (length ',typespeclist)))
                  (setf ,accessor (nth ,i ',typespeclist))
                  (when (slot-boundp object ,accessor)
                    (when (typep (slot-value object ,accessor) '(simple-array character))
                      (setf (slot-value object ,accessor) (ironclad:hex-string-to-byte-array (slot-value object ,accessor))))
                    (when (typep (slot-value object ,accessor) 'integer)
                      (setf (slot-value object ,accessor) (ironclad:integer-to-octets (slot-value object ,accessor)))))))))

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

;; Helpers

(defun generate-reader-functions (class slot)
  `(progn
     (defgeneric ,slot (object &key &allow-other-keys)
       (:documentation "General octet reader function. Keys: :int :hex-string. Default is 'octet-vector"))

     (defmethod ,slot :around ((object ,class) &key int hex-string)
                (let ((result (call-next-method)))
                  (when (equal int t)
                    (assert (typep result 'octet-vector))
                    (return-from ,slot (ironclad:octets-to-integer result)))
                  (when (equal hex-string t)
                    (assert (typep result 'octet-vector))
                    (return-from ,slot (ironclad:byte-array-to-hex-string result)))
                  result))
     (defmethod ,slot ((object ,class) &key)
       (slot-value object ',slot))))



(defun generate-composite-reader-functions (class name &rest slots)
  `(progn

     (defgeneric ,name (object &key int hex-string &allow-other-keys)
       (:documentation "General octet reader function. Keys: :int :hex-string. Default is 'octet-vector"))

     (defmethod ,name ((object ,class) &key)
       (concatenate 'octet-vector
                     ,@ (mapcar #'(lambda (slot) `(,slot object)) (car slots))))

     (defmethod ,name :around ((object ,class) &key int hex-string)
                (let ((result (call-next-method)))
                  (when (equal int t)
                    (assert (typep result 'octet-vector))
                    (return-from ,name (ironclad:octets-to-integer result)))
                  (when (equal hex-string t)
                    (assert (typep result 'octet-vector))
                    (return-from ,name (ironclad:byte-array-to-hex-string result)))
                  result))))




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
