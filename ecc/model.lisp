;;;; model.lisp
(in-package #:cl-ecc)

;; Model

;; Generic Classes and reader functions
(defclass Key () ())
(defclass Private-Key (Key)
  ((key :initarg :key :reader key :type octet-vector)))
(defclass Public-key (Key) ())

(define-slot-to-octet-vector-parser Private-Key key)

(defclass Point ()
  ((x :initarg :x :reader x :type (octet-vector))
   (y :initarg :y :reader y :type octet-vector)))
(define-slot-to-octet-vector-parser Point x y )

(defclass Curve ()
  ((a :initarg :a :reader a :type octet-vector
      :initform (error 'unbound-slot :msg  ":a must be initialized"))
   (b :initarg :b :reader b :type octet-vector
      :initform (error 'unbound-slot :msg ":b must be initialized"))
   (p :initarg :p :reader p :type octet-vector
      :initform (error 'unbound-slot :msg ":p must be initialized"))
   (g :initarg :g :reader g :type Point
      :initform (error 'unbound-slot :msg ":g must be initialized"))
   (n :initarg :n :reader n :type octet-vector
      :initform (error 'unbound-slot :msg ":n must be initialized"))
   (h :initarg :h :reader h :type octet-vector
      :initform (error 'unbound-slot :msg ":h must be initialized"))))
(define-slot-to-octet-vector-parser  Curve a b p n h )

(defvar *inf-point* (make-instance 'Point
                                            :x (octet-vector 1)
                                            :y (octet-vector 1))
   "Zero Point")
