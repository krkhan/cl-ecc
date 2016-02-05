;;;; model-macros.lisp
(in-package :cl-ecc)

;; Curve object macros
(defmacro define-elliptic-curve (name sym &key a b p g n h)
  (let* ((curve-class-name (conc-to-symbol 'Curve-  name))
         (point-class-name (conc-to-symbol 'Point- name))
         (point-coord-length (max 2 (length n)))
         (point-coord-size (max 1 point-coord-length)))
    `(progn
       (defclass ,point-class-name (Point)
         ((x :initarg  :x :reader x :type (octet-vector ,point-coord-size)
             :initform (error 'unbound-slot :msg  ":x must be initialized"))
          (y :initarg :y :reader y :type (octet-vector ,point-coord-size)
             :initform (error 'unbound-slot :msg  ":y must be initialized"))))
       (defclass ,curve-class-name (Curve) ())
       (defvar ,sym
         (make-instance ',curve-class-name
                        :a ,a :b ,b :p ,p :n ,n :h ,h
                        :g (make-instance ',point-class-name
                                          :x (subseq ,g 0 ,point-coord-length )
                                          :y (subseq ,g ,point-coord-length)))))))


;; Object slot functions macros
(defmacro define-composite-octet-reader-functions (class &rest args)
  "Macro: Arglist: Class &rest (name1 fn1 fn2 ...) (name2 fn3 fni ...)
   Genererates methods on the class named as the first argument of
   all lists in 'rest'. The new methods returns the concatenated
   result of the rest of the arguments on the object.
   Return type is (simple-array (unsigned-byte (*))).
   All parts need to be this type."
  `(progn
     ,@ (mapcar #'(lambda (fn-def)
                    (let ((fn (first fn-def))
                          (fn-slots (rest fn-def)))
                      (mapcan #'(lambda (slots)
                                  (generate-composite-reader-functions class fn slots))
                              (list fn-slots))))
                args)))

(defun generate-composite-reader-functions (class name &rest slots)
  "Helper function for define-composite-octet-reader-functions."
  `(defmethod ,name ((object ,class))
       (concatenate 'octet-vector
                    ,@ (mapcar #'(lambda (slot) `(,slot object)) (car slots)))))

(defmacro define-slot-to-octet-vector-parser (classname &rest slots)
  "Macro: Argelist: classname &rest slots
   Defines an :after method on initialize-instance where it will
   convert all bound slots from either (simple-array character) or 'integer'
   into (simple-array (unsigned-byte 8 (*)"

  (let ((i (gensym))
        (accessor (gensym)))
    `(defmethod initialize-instance :after ((object ,classname) &key)
                (do ((,i 0 (+ ,i 1))
                     (,accessor))
                    ((>= ,i (length ',slots)))
                  (setf ,accessor (nth ,i ',slots))
                  (when (slot-boundp object ,accessor)
                    (when (typep (slot-value object ,accessor)
                                 '(simple-array character))
                      (setf (slot-value object ,accessor)
                            (ironclad:hex-string-to-byte-array
                             (slot-value object ,accessor))))
                    (when (typep (slot-value object ,accessor) 'integer)
                      (setf (slot-value object ,accessor)
                            (ironclad:integer-to-octets
                             (slot-value object ,accessor)))))))))

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
                             :msg
                             (format nil "~a must be of type ~a instead of ~a. The program will parse integers and hex-strings to correct type."
                                     ,accessor ,typename
                                     (type-of (slot-value object ,accessor)))))
                   instance)))))
