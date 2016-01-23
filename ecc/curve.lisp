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

(defgeneric point->int (pt)
  (:documentation "Return: integer. concatenate x and y coords of point into integer"))



(defmethod point-equalp ((p1 Point) (p2 Point))
  (and
    (equalp (x p1) (x p2))
    (equalp (y p1) (y p2))))


(defmethod valid-curve-p ((ec Curve))
  (with-slots-to-integers (ec-a ec-b ec-p) (a b p) ec
    (assert (and
             (<= 0 ec-a)
             (< ec-a ec-p)
             (< 0 ec-b)
             (< ec-b ec-p)
             (> ec-p 2)))
    (assert (not
             (= 0 (add-mod (mul-mod 4 (expt-mod ec-a 3 ec-p) ec-p)
                           (mul-mod 27 (expt-mod ec-b 2 ec-p) ec-p)
                           ec-p))))))

(defmethod point-on-curve-p ((ec Curve) (pt Point))
  (with-slots-to-integers (ec-a ec-b ec-p) (a b p) ec
    (with-slots-to-integers (pt-x pt-y) (x y) pt
      (when (point-equalp pt *inf-point*)
        (return-from point-on-curve-p t))
      (let ((left (expt-mod pt-y 2 ec-p))
            (right (add-mod (expt-mod pt-x 3 ec-p)
                            (mul-mod ec-a pt-x ec-p)
                            ec-b
                            ec-p)))
        (equalp left right)))))

(defmethod at-x ((ec Curve) (x integer))
  (with-slots-to-integers (a-val b-val p-val) (a b p) ec
      (assert (< x p-val))
      (let* ((ysq (add-mod (expt-mod x 3 p-val) (mul-mod a-val x p-val) b-val p-val))
            (y (sqrt-mod ysq p-val)))
        (values y (- p-val y)))))

(defmethod point-inverse ((ec Curve) (pt Point))
  (with-slots-to-integers (p-val) (p) ec
    (with-slots-to-integers (y-val) (y) pt
      (make-instance (type-of pt)
                     :x (x pt)
                     :y (read-value 'byte-array (ironclad:integer-to-octets
                                                 (mul-mod -1 y-val p-val))
                                    :bytes (length (y pt)))))))

(defmethod add-points ((ec Curve) (p1 Point) (p2 Point))
  (with-slots-to-integers (ec-a ec-p) (a p) ec
    (with-slots-to-integers (p1-x p1-y) (x y) p1
      (with-slots-to-integers (p2-x p2-y) (x y) p2
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
              (setf s (mul-mod (add-mod (mul-mod 3 p1-x p1-x ec-p)
                                        ec-a
                                        ec-p)
                               (inv-mod (mul-mod 2 p1-y ec-p)
                                        ec-p)
                               ec-p))
              (setf s (mul-mod (sub-mod p2-y p1-y ec-p)
                               (inv-mod (sub-mod p2-x p1-x ec-p)
                                        ec-p)
                               ec-p)))
          (setf result-x (sub-mod (mul-mod s s ec-p)
                                  p1-x
                                  p2-x
                                  ec-p))
          (setf result-y (sub-mod (mul-mod s
                                           (sub-mod p1-x result-x ec-p)
                                           ec-p)
                                  p1-y
                                  ec-p))
          (make-instance (type-of p1)
                         :x (read-value 'byte-array (ironclad:integer-to-octets result-x))
                         :y (read-value 'byte-array (ironclad:integer-to-octets result-y))))))))


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

(defmethod point->int ((pt Point))
  (ironclad:octets-to-integer (concatenate '(vector (unsigned-byte 8)) (x pt) (y pt))))
