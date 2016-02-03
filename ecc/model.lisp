;;;; model.lisp

(in-package #:cl-ecc)

;; Model

(defmacro define-elliptic-curve (name sym &key par-a par-b par-p par-g par-n par-h)
    (let ((curve-class-name (intern (concatenate 'string (string 'Curve-) (string name))))
          (point-class-name (intern (concatenate 'string (string 'Point-) (string name))))
          (point-size (/ (length par-n) 2)))
      `(progn
         (defclass ,point-class-name (Point)
           ((x :initarg  :x
               :type '(octet-vector ,point-size))
            (y :initarg :y
               :type '(octet-vector ,point-size))))
          (defclass ,curve-class-name (Curve) ())

          (defvar ,sym
            (make-instance ',curve-class-name
                           :a (ironclad:hex-string-to-byte-array ,par-a)
                           :b (ironclad:hex-string-to-byte-array ,par-b)
                           :p (ironclad:hex-string-to-byte-array ,par-p)
                           :g (make-instance ',point-class-name
                                             :x (ironclad:hex-string-to-byte-array (subseq ,par-g 0 (/ (length ,par-g) 2)))
                                             :y (ironclad:hex-string-to-byte-array (subseq ,par-g (/ (length ,par-g) 2))))
                           :n (ironclad:hex-string-to-byte-array ,par-n)
                           :h (ironclad:hex-string-to-byte-array ,par-h))))))


;; Generic Classes and reader functions
(defclass Key () ())

(defclass Private-Key (Key)
  ((key :initarg :key
        :type 'octet-vector)))

(define-custom-octet-reader-functions Private-Key key)
(define-slot-to-octet-vector-parser Private-key key)


(defclass Public-key (Key) ())

(defclass Point ()
  ((x :initarg :x
      :type '(octet-vector))
   (y :initarg :y
      :type 'octet-vector)))

(define-custom-octet-reader-functions Point x y)
(define-slot-to-octet-vector-parser Point x y )



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
   (g :reader g
      :initarg :g
      :initform (error 'unbound-slot :msg ":g must be initialized")
      :type 'Point)
   (n :initarg :n
      :initform (error 'unbound-slot :msg ":n must be initialized")
      :type 'octet-vector)
   (h :initarg :h
      :initform (error 'unbound-slot :msg ":h must be initialized")
      :type 'octet-vector)))
(define-slot-to-octet-vector-parser  Curve a b p n h )


;; Reader functions
(define-custom-octet-reader-functions Curve a b p n h)
(define-slot-to-octet-vector-parser Curve a b p n h)


(defvar *inf-point* (make-instance 'Point
                                   :x (octet-vector 1)
                                   :y (octet-vector 1))
  "Zero Point")
