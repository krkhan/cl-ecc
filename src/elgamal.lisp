;;;; elgamal.lisp
;;;; https://en.wikipedia.org/wiki/ElGamal_encryption

(in-package #:cl-ecc)

(defclass ElGamalMessage ()
  ((c1 :initarg :c1 :accessor c1)
   (c2 :initarg :c2 :accessor c2)))

(defgeneric elgamal-encrypt (curve plaintext partner-pub my-priv)
  (:documentation "Returns: 'ElGamalMessage"))

(defgeneric elgamal-decrypt (curve ElGamalMessage my-priv)
  (:documentation "Returns: a 'Point."))

(defmethod elgamal-encrypt ((ec Curve) (plaintext Point)
                            (partner-pub Point) (my-priv integer))
  (assert (point-on-curve-p ec plaintext))
  (assert (point-on-curve-p ec partner-pub))
  (let ((my-pub (ecdh-gen-pub ec my-priv)))
    (make-instance 'ElGamalMessage
                   :c1 my-pub
                   :c2 (add-points ec
                                   plaintext
                                   (ecdh-gen-secret ec my-priv partner-pub)))))

(defmethod elgamal-decrypt ((ec Curve) (m ElGamalMessage) (my-priv integer))
  (assert (point-on-curve-p ec (c1 m)))
  (assert (point-on-curve-p ec (c2 m)))
  (add-points ec
              (c2 m)
              (point-inverse ec (ecdh-gen-secret ec my-priv (c1 m)))))
