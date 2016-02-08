;;;; curve.lisp

(in-package #:cl-ecc)

;; Generics
(defgeneric point-equalp (pt1 pt2)
  (:documentation "Result: Predicate. Compares two points structures"))
(defgeneric valid-curve-p (curve)
  (:documentation "Result: Predicate. Test if ec curve is valid by params"))
(defgeneric point-on-curve-p (curve pt)
  (:documentation "Result: Predicate. Test if a given point is valid on
                   the given curve"))
(defgeneric at-x (curve x)
  (:documentation "NEED TO CHECK"))
(defgeneric point-inverse (curve pt)
  (:documentation "Result: a 'Point. Y coordinates inverted over x-axis"))
(defgeneric add-points (curve pt1 pt2)
  (:documentation "Result: a 'Point. Addition of 2 Points on a Curve"))
(defgeneric mul-point (curve pt int)
  (:documentation "Result: a 'Point. Multiplication of 2 Points on a Curve"))
(defgeneric order-of-point (curve pt)
  (:documentation "Return: integer. Order of a Point on a Curve"))
(defgeneric point->pubkey (pt &key version-byte)
  (:documentation "Return: integer. concatenate x and y coords of point into integer"))

;; Methods
(defmethod point-equalp ((p1 Point) (p2 Point))
  (with-accessors ((x1 x) (y1 y)) p1
    (with-accessors ((x2 x) (y2 y)) p2
      (and
       (= x1 x2)
       (= y1 y2)))))

(defmethod valid-curve-p ((ec Curve))
  (with-accessors ((a a) (b b) (p p)) ec
        (assert (and (<= 0 a) (< a p) (<= 0 b) (< b p) (> p 2)))
        (assert (not (= 0 (add-mod (mul-mod 4 (ironclad:expt-mod a 3 p) p)
                                   (mul-mod 27 (ironclad:expt-mod b 2 p) p)
                               p))))
        t))

(defmethod point-on-curve-p ((ec Curve) (pt Point))
  (with-accessors ((x x) (y y)) pt
    (with-accessors ((a a) (b b) (p p)) ec
          (when (point-equalp pt *inf-point*)
            (return-from point-on-curve-p t))
          (let ((left (ironclad:expt-mod y 2 p))
                (right (add-mod (ironclad:expt-mod x 3 p) (mul-mod a x p) b p)))
              (equalp left right)))))

(defmethod at-x ((ec Curve) (x integer))
  (with-accessors ((a a) (b b) (p p)) ec
        (assert (< x p))
        (let* ((ysq (add-mod (ironclad:expt-mod x 3 p) (mul-mod a x p) b p))
               (y (sqrt-mod ysq p)))
          (values y (- p y)))))

(defmethod point-inverse ((ec Curve) (pt Point))
  (with-accessors ((p p)) ec
    (with-accessors ((y y)) pt
          (make-instance (type-of pt)
                         :x (x pt)
                         :y (mul-mod -1 y p)))))

(defmethod add-points ((ec Curve) (pt1 Point) (pt2 Point))
  (with-accessors ((x1 x) (y1 y)) pt1
    (with-accessors ((x2 x) (y2 y)) pt2
      (with-accessors ((a a) (p p) (g g)) ec
          (when (point-equalp pt1 *inf-point*)(return-from add-points pt2))
          (when (point-equalp pt2 *inf-point*) (return-from add-points pt1))
          (when (and (= x1 x2) (or (not (= y1 y2))
                                   (= y1 0)))
            (return-from add-points *inf-point*))
          (let (s result-x result-y)
            (if (= x1 x2)
                (setf s (mul-mod (add-mod (mul-mod 3 x1 x1 p) a p)
                                 (inv-mod (mul-mod 2 y1 p) p)
                                 p))
                (setf s (mul-mod (sub-mod y2 y1 p)
                                 (inv-mod (sub-mod x2 x1 p) p)
                                 p)))
            (setf result-x (sub-mod (mul-mod s s p) x1 x2 p))
            (setf result-y (sub-mod (mul-mod s (sub-mod x1 result-x p)p) y1 p))
            (make-instance (type-of g)
                           :x  result-x
                           :y  result-y ))))))


(defmethod mul-point ((ec Curve) (pt Point) (multiplier integer))
  (when (= 0 multiplier) (return-from mul-point *inf-point*))
  (loop
     with result = pt
     for i from (- (integer-length multiplier) 2) downto 0
     do
       (setf result (add-points ec result result))
       (when (logbitp i multiplier)
         (setf result (add-points ec result pt)))
     finally (return-from mul-point result)))

(defmethod order-of-point ((c Curve) (pt Point))
  (loop
     for i from 1 and
     result = (mul-point c pt i)
     until (point-equalp result *inf-point*)
     finally (return-from order-of-point (1- i))))
