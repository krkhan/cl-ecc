;;;; secp256k1.lisp

(in-package #:cl-ecc.curve-parameters)

;; These points have NOT been verified. Only circular proofs.

(defvar *secp256k1-gen*
  (make-instance
   'Point
   :x #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
   :y #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8))


(defvar *secp256k1-curve*
  (make-instance
   'Curve
   :a 0
   :b 7
   :p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
   :g *secp256k1-gen*
   :n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141))

(export '*secp256k1-gen*)
(export '*secp256k1-curve*)
