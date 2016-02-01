;;;; ecdh.lisp

(in-package #:cl-ecc)

(defgeneric ecdh-gen-pub (curve priv-key)
  (:documentation "Result: a ECDSA-Public-Key (Point)."))
(defgeneric ecdh-gen-secret (curve my-priv partner-pub)
  (:documentation "Result: ECDSA-Secret (Point)."))

(defclass ECDH-Public-Key (Point) ())
(defclass ECDH-Secret (Point) ())

(defmethod ecdh-gen-pub ((ec Curve) (priv Private-Key))
  (assert (and (< 0 priv) (< (get-slot :int 'key priv) (get-slot :int 'n ec))))
  (change-class (mul-point ec (get-slot :point 'g ec) (get-slot :int 'key priv))
                'ECDH-Public-Key))

(defmethod ecdh-gen-secret ((ec Curve) (my-priv Private-Key) (partner-pub ECDH-Public-Key))
  (assert (point-on-curve-p ec partner-pub))
  (change-class (mul-point ec partner-pub (get-slot :int 'key my-priv))
                'ECDH-Secret))
