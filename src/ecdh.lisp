;;;; ecdh.lisp

(in-package #:cl-ecc.ecdh)

(defmethod ecdh-gen-pub ((c Curve) (priv integer))
  (assert (and (< 0 priv) (< priv (n c))))
  (mul-point c (g c) priv))

(defmethod ecdh-gen-secret ((c Curve) (my-priv integer) (partner-pub Point))
  (assert (point-on-curve-p c partner-pub))
  (mul-point c partner-pub my-priv))
