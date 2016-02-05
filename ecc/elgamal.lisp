;;;; elgamal.lisp
;;;; https://en.wikipedia.org/wiki/ElGamal_encryption

(in-package #:cl-ecc)

;; Generics
(defgeneric elgamal-encrypt (curve plaintext partner-pub my-priv)
  (:documentation "Returns: 'ElGamalMessage"))
(defgeneric elgamal-decrypt (curve ElGamalMessage my-priv)
  (:documentation "Returns: a 'Point."))

;; Classes
(defclass ElGamalMessage ()
  ((x :initarg :x
      :reader x
      :type ECDSA-Public-Key)
   (y :initarg :y
      :reader y
      :type Elgamalciphertext)))
(defclass ElGamalPlaintext (Point)())
(defclass ElGamalCiphertext (Point) ())

;; Methods
(defmethod elgamal-encrypt ((c Curve) (plaintext ELGamalPlaintext)
                            (partner-pub ECDH-Public-Key) (my-priv ECDH-Private-Key))
  (assert (point-on-curve-p c plaintext))
  (assert (point-on-curve-p c partner-pub))
  (let* ((my-pub (ecdh-gen-pub c my-priv))
         (secret-point (ecdh-gen-secret c my-priv partner-pub))
         (ciphertext-point (add-points c plaintext secret-point))
         (ciphertext-instance (make-instance 'ElGamalCiphertext
                                             :x (x ciphertext-point)
                                             :y (y ciphertext-point))))
    (make-instance 'ElGamalMessage :x my-pub :y ciphertext-instance)))

(defmethod elgamal-decrypt ((ec Curve) (msg ElGamalMessage) (my-priv ECDH-Private-Key))
  (let* ((partner-pub (x msg))
         (ciphertext (y msg))
         (secret-point (ecdh-gen-secret ec my-priv partner-pub))
         (plaintext (add-points ec ciphertext (point-inverse ec secret-point))))
    (assert (point-on-curve-p ec partner-pub))
    (assert (point-on-curve-p ec ciphertext))
    (make-instance 'ElGamalPlaintext :x (x plaintext) :y (y plaintext))))
