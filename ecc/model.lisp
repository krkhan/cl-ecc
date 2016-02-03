;;;; model.lisp

(in-package #:cl-ecc)

(defgeneric get-slot (spec slot object)
  (:documentation
   "General reader function that gets value from object according to spec.
    Arguments; spec: :int, :vector, :hex-string, :point"))

(defgeneric get-key (spec object)
  (:documentation
   "Get key from object and returns according to spec.
    Arguments for spec: :int, :vector, :hex-string"))


;; Model

(defmacro define-elliptic-curve (name sym &key par-a par-b par-p par-g par-n par-h)
    (let ((curve-class-name (intern (concatenate 'string (string 'Curve-) (string name))))
          (point-class-name (intern (concatenate 'string (string 'Point-) (string name)))))
      `(progn
          (defclass ,point-class-name (Point) ())
          (defclass ,curve-class-name (Curve) ())

          (defvar ,sym
            (make-instance ',curve-class-name
                           :a (hex-string-to-byte-array ,par-a)
                           :b (hex-string-to-byte-array ,par-b)
                           :p (hex-string-to-byte-array ,par-p)
                           :g (make-instance ',point-class-name
                                             :x (hex-string-to-byte-array (subseq ,par-g 0 (/ (length ,par-g) 2)))
                                             :y (hex-string-to-byte-array (subseq ,par-g (/ (length ,par-g) 2))))
                           :n (hex-string-to-byte-array ,par-n)
                           :h (hex-string-to-byte-array ,par-h))))))


;; Generic Classes and reader functions
(defclass Key ()
  ((key :initarg :key
        :initform -1
        :type 'octet-vector)))

(defclass Private-Key (Key) ())
(defclass Public-key (Key) ())

(define-slot-type-parser Key
    key octet-vector)
(define-slot-type-validator Key
    key octet-vector)

(defclass Point ()
  ((x :initarg :x
      :initform (error 'unbound-slot :msg ":x must be initialized")
      :type '(octet-vector))
   (y :initarg :y
      :initform (error 'unbound-slot :msg  ":y must be initialized")
      :type 'octet-vector)))
(define-slot-type-parser Point
    x octet-vector
    y octet-vector)
(define-slot-type-validator Point
    x octet-vector
    y octet-vector)

(defclass Curve ()
  ((a :initarg :a
      :initform (error 'unbound-slot :msg  ":a must be initialized")
      :type 'octet-vector)
   (b :initarg :b
      :initform (error 'unbound-slot :msg ":b must be initialized")
      :type 'octet-vector)
   (p :initarg :p
      :initform (error 'unbound-slot :msg ":p must be initialized")
      :type 'octet-vector)
   (g :initarg :g
      :initform (error 'unbound-slot :msg ":g must be initialized")
      :type 'Point)
   (n :initarg :n
      :initform (error 'unbound-slot :msg ":n must be initialized")
      :type 'octet-vector)
   (h :initarg :h
      :initform (error 'unbound-slot :msg ":h must be initialized")
      :type 'octet-vector)))
(define-slot-type-parser  Curve
    a octet-vector
    b octet-vector
    p octet-vector
    g Point
    n octet-vector
    h octet-vector)
(define-slot-type-validator Curve
    a octet-vector
    b octet-vector
    p octet-vector
    g Point
    n octet-vector
    h octet-vector)

;; Reader functions
(define-generic-reader-functions Key)
(define-generic-reader-functions Point)
(define-generic-reader-functions Curve)


(defvar *inf-point* (make-instance 'Point
                                   :x (octet-vector 1)
                                   :y (octet-vector 1))
  "Zero Point")
