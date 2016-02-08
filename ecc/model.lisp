;;;; model.lisp
(in-package #:cl-ecc)

;; Model

;; Generic Classes and reader functions
(defclass Size ()
  ((bytes :initarg :bytes :reader bytes :type integer)))
(defclass Key () ())
(defclass Private-Key (Key Size)
  ((key :initarg :key :reader key :type integer)))
(defclass Public-key (Key Size) ())

(defclass Point (Size)
  ((x :initarg :x :reader x :type integer)
   (y :initarg :y :reader y :type integer)))

(defclass Curve ()
  ((a :initarg :a :reader a :type integer
      :initform (error 'unbound-slot :msg  ":a must be initialized"))
   (b :initarg :b :reader b :type integer
      :initform (error 'unbound-slot :msg ":b must be initialized"))
   (p :initarg :p :reader p :type integer
      :initform (error 'unbound-slot :msg ":p must be initialized"))
   (g :initarg :g :reader g :type Point
      :initform (error 'unbound-slot :msg ":g must be initialized"))
   (n :initarg :n :reader n :type integer
      :initform (error 'unbound-slot :msg ":n must be initialized"))
   (h :initarg :h :reader h :type integer
      :initform (error 'unbound-slot :msg ":h must be initialized"))))

(defvar *inf-point* (make-instance 'Point :x 0 :y 0)  "Zero Point")
