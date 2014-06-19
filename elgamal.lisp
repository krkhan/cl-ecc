(in-package :ecc)

(defclass ElGamalMessage ()
  ((c1 :initarg :c1 :accessor c1)
   (c2 :initarg :c2 :accessor c2)))

(defmethod elgamal-encrypt ((c Curve) (plaintext Point) (partner-pub Point) (my-priv integer))
  (assert (point-on-curve-p c plaintext))
  (assert (point-on-curve-p c partner-pub))
  (let ((my-pub (ecdh-gen-pub c my-priv)))
    (make-instance
      'ElGamalMessage
      :c1 my-pub
      :c2 (add-points
            c
            plaintext
            (ecdh-gen-secret c my-priv partner-pub)))))

(defmethod elgamal-decrypt ((c Curve) (m ElGamalMessage) (my-priv integer))
  (assert (point-on-curve-p c (c1 m)))
  (assert (point-on-curve-p c (c2 m)))
  (add-points
    c
    (c2 m)
    (point-inverse c (ecdh-gen-secret c my-priv (c1 m)))))
