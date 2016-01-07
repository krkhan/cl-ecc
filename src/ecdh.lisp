;;;; ecdh.lisp

(in-package #:cl-ecc.ecdh)

(defmethod ecdh-gen-pub ((c Curve) (priv integer))
  (assert (and (< 0 priv) (< priv (n c))))
  (mul-point c (g c) priv))

(defmethod ecdh-gen-secret ((c Curve) (my-priv integer) (partner-pub Point))
  (assert (point-on-curve-p c partner-pub))
  (mul-point c partner-pub my-priv))

(defvar *p17-curve* (make-instance 'Curve
                                   :a 2
                                   :b 2
                                   :p 17
                                   :g (make-instance
                                       'Point
                                       :x 5
                                       :y 1)
                                   :n 19))
