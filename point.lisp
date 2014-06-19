(in-package :ecc)

(defclass Point ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)))

(validate-accessor-types Point x integer y integer)

(defconstant *inf-point* (make-instance 'Point :x 0 :y 0))

(defmethod point-equalp ((p1 Point) (p2 Point))
  (and
    (= (x p1) (x p2))
    (= (y p1) (y p2))))

(defmethod print-object ((p Point) out)
  (when (point-equalp p *inf-point*)
    (format out "<Point At Infinity>")
    (return-from print-object))
  (format out "<Point (~x, ~x)>" (x p) (y p)))
