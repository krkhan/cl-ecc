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


(defmethod point-equalp ((p1 Point) (p2 Point))
  (and
   (= (get-slot :int 'x p1) (get-slot :int 'x p2))
   (= (get-slot :int 'y p1) (get-slot :int 'y p2))))


(defmethod valid-curve-p ((ec Curve))
  (let ((a (get-slot :int 'a ec ))
        (b (get-slot :int 'b ec))
        (p (get-slot :int 'p ec)))
    (assert (and
             (<= 0 a)
             (< a p)
             (<= 0 b)
             (< b p)
             (> p 2)))
    (assert (not
             (= 0 (add-mod (mul-mod 4 (ironclad:expt-mod a 3 p) p)
                           (mul-mod 27 (ironclad:expt-mod b 2 p) p)
                           p))))
    t))

(defmethod point-on-curve-p ((ec Curve) (pt Point))
  (let ((x (get-slot :int 'x pt))
        (y (get-slot :int 'y pt))
        (p (get-slot :int 'p ec))
        (a (get-slot :int 'a ec))
        (b (get-slot :int 'b ec)))
    (when (point-equalp pt *inf-point*)
      (return-from point-on-curve-p t))
    (let ((left (ironclad:expt-mod y 2 p))
          (right (add-mod (ironclad:expt-mod x 3 p)
                          (mul-mod a x p)
                          b
                          p)))
      (equalp left right))))

(defmethod at-x ((ec Curve) (x integer))
  (let ((a (get-slot :int 'a ec))
        (b (get-slot :int 'b ec))
        (p (get-slot :int 'p ec)))
    (assert (< x p))
    (let* ((ysq (add-mod (ironclad:expt-mod x 3 p) (mul-mod a x p) b p))
           (y (sqrt-mod ysq p)))
      (values y (- p y)))))

(defmethod point-inverse ((ec Curve) (pt Point))
  (let ((y (get-slot :int 'y pt))
        (p (get-slot :int 'p ec)))
    (make-instance (type-of pt)
                   :x (get-slot :vector 'x pt)
                   :y (ironclad:integer-to-octets
                       (mul-mod -1 y p)))))

(defmethod add-points ((ec Curve) (p1 Point) (p2 Point))
  (let ((p1-x (get-slot :int 'x p1))
        (p1-y (get-slot :int 'y p1))
        (p2-x (get-slot :int 'x p2))
        (p2-y (get-slot :int 'y p2))
        (p (get-slot :int 'p ec))
        (a (get-slot :int 'a ec)))
    (when (point-equalp p1 *inf-point*)
      (return-from add-points p2))
    (when (point-equalp p2 *inf-point*)
      (return-from add-points p1))
    (when (and (= p1-x p2-x)
               (or (not (= p1-y p2-y))
                   (= p1-y 0)))
      (return-from add-points *inf-point*))

    (let (s result-x result-y)
      (if (= p1-x p2-x)
          (setf s (mul-mod (add-mod (mul-mod 3 p1-x p1-x p)
                                    a
                                    p)
                           (inv-mod (mul-mod 2 p1-y p)
                                    p)
                           p))
          (setf s (mul-mod (sub-mod p2-y p1-y p)
                           (inv-mod (sub-mod p2-x p1-x p)
                                    p)
                           p)))
      (setf result-x (sub-mod (mul-mod s s p)
                              p1-x
                              p2-x
                              p))
      (setf result-y (sub-mod (mul-mod s
                                       (sub-mod p1-x result-x p)
                                       p)
                              p1-y
                              p))
      (make-instance 'Point
                     :x (ironclad:integer-to-octets result-x)
                     :y (ironclad:integer-to-octets result-y)))))


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
