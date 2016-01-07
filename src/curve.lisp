;;;; curve.lisp

(in-package #:cl-ecc.curve)

;; Point Structures

(def-exporting-class Point ()
  ((x :accessor x :initarg :x :initform 0 :export t)
   (y :accessor y :initarg :y :initform 0 :export t)))

(validate-accessor-types Point x integer y integer)

;; using defconstant was uncompatible with sbcl
(defvar *inf-point* (make-instance 'Point :x 0 :y 0))

(defmethod point-equalp ((p1 Point) (p2 Point))
  (and
    (= (x p1) (x p2))
    (= (y p1) (y p2))))

(defmethod print-object ((p Point) out)
  (when (point-equalp p *inf-point*)
    (format out "<Point At Infinity>")
    (return-from print-object))
  (format out "<Point (~x, ~x)>" (x p) (y p)))


;; Curve Structures

(def-exporting-class Curve ()
  ((a :accessor a :initarg :a :export t)
   (b :accessor b :initarg :b :export t)
   (p :accessor p :initarg :p :export t)
   (g :accessor g :initarg :g :export t)
   (n :accessor n :initarg :n :export t)))

(validate-accessor-types Curve
                         a integer
                         b integer
                         p integer
                         g Point
                         n integer)

(defmethod print-object ((c Curve) out)
  (format out "<Curve a:~a b:~a p:~a g:~a n:~a>"
          (a c) (b c) (p c) (g c) (n c)))

(defmethod valid-curve-p ((c Curve))
  (assert (and
            (< 0 (a c))
            (< (a c) (p c))
            (< 0 (b c))
            (< (b c) (p c))
            (> (p c) 2)))
  (assert (not
            (= 0
              (add-mod
                (mul-mod 4 (expt-mod (a c) 3 (p c)) (p c))
                (mul-mod 27 (expt-mod (b c) 2 (p c)) (p c))
                (p c))))))

(defmethod point-on-curve-p ((c Curve) (pt Point))
  (when (point-equalp pt *inf-point*)
    (return-from point-on-curve-p t))
  (let (left right)
    (setf left
      (expt-mod (y pt) 2 (p c)))
    (setf right
      (add-mod
        (expt-mod (x pt) 3 (p c))
        (mul-mod
          (a c)
          (x pt)
          (p c))
        (b c)
        (p c)))
    (equalp left right)))

(defmethod at-x ((c Curve) (x integer))
  (assert (< x (p c)))
  (let (ysq y)
    (setf ysq
      (add-mod
        (expt-mod x 3 (p c))
        (mul-mod
          (a c)
          x
          (p c))
        (b c)
        (p c)))
    (setf y (sqrt-mod ysq (p c)))
    (values y (- (p c) y))))

(defmethod point-inverse ((c Curve) (pt Point))
  (make-instance 'Point :x (x pt) :y (mul-mod -1 (y pt) (p c))))

(defmethod add-points ((c Curve) (p1 Point) (p2 Point))
  (when (point-equalp p1 *inf-point*)
    (return-from add-points p2))
  (when (point-equalp p2 *inf-point*)
    (return-from add-points p1))
  (when (and
          (= (x p1) (x p2))
          (or
            (not (= (y p1) (y p2)))
            (= (y p1) 0)))
    (return-from add-points *inf-point*))

  (let (s result-x result-y)
    (if (= (x p1) (x p2))
      (setf s
        (mul-mod
          (add-mod
            (mul-mod 3 (x p1) (x p1) (p c))
            (a c)
            (p c))
          (inv-mod
            (mul-mod 2 (y p1) (p c))
            (p c))
          (p c)))
      (setf s
        (mul-mod
          (sub-mod (y p2) (y p1) (p c))
          (inv-mod (sub-mod (x p2) (x p1) (p c)) (p c))
          (p c))))
    (setf result-x
      (sub-mod
        (mul-mod s s (p c))
        (x p1)
        (x p2)
        (p c)))
    (setf result-y
      (sub-mod
        (mul-mod
          s
          (sub-mod (x p1) result-x (p c))
          (p c))
        (y p1)
        (p c)))
    (make-instance 'Point :x result-x :y result-y)))

(defmethod mul-point ((c Curve) (pt Point) (d integer))
  (when (= 0 d)
    (return-from mul-point *inf-point*))

  (loop with result = pt
        for i from (- (integer-length d) 2) downto 0
        do
          (setf result (add-points c result result))
          (when (logbitp i d)
            (setf result (add-points c result pt)))
        finally
          (return-from mul-point result)))

(defmethod order-of-point ((c Curve) (pt Point))
  (loop for i from 1 and
        result = (mul-point c pt i)
        until (point-equalp result *inf-point*)
        finally (return-from order-of-point (1- i))))
