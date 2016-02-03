;;;; elgamal.lisp
;;;; https://en.wikipedia.org/wiki/ElGamal_encryption

(in-package #:cl-ecc)

;; Elgamal generics

(defgeneric elgamal-encrypt (curve plaintext partner-pub my-priv)
  (:documentation "Returns: 'ElGamalMessage"))
(defgeneric elgamal-decrypt (curve ElGamalMessage my-priv)
  (:documentation "Returns: a 'Point."))

;; Elgamal classes

(defclass ElGamalMessage ()
  ((x :initarg :x
      :type 'ECDSA-Public-Key)
   (y :initarg :y
      :type 'Elgamalciphertext)))

(define-custom-octet-reader-functions ElGamalMessage x y)

(defclass ElGamalPlaintext (Point)())
(defclass ElGamalCiphertext (Point) ())

;; ElGamal methods

(defmethod elgamal-encrypt ((c Curve) (plaintext ELGamalPlaintext)
                            (partner-pub ECDH-Public-Key) (my-priv ECDH-Private-Key))

  (assert (point-on-curve-p c plaintext))
  (assert (point-on-curve-p c partner-pub))
  (let ((my-pub (ecdh-gen-pub c my-priv)))
    (make-instance
     'ElGamalMessage
     :x my-pub
     :y (change-class (add-points c
                                  plaintext
                                  (ecdh-gen-secret c my-priv partner-pub))
                      'ElGamalCiphertext))))

(defmethod elgamal-decrypt ((c Curve) (msg ElGamalMessage) (my-priv ECDH-Private-Key))
  (let ((partner-pub (x msg))
        (ciphertext (y msg)))
    (assert (point-on-curve-p c partner-pub))
    (assert (point-on-curve-p c ciphertext))
    (change-class (add-points c
                              ciphertext
                              (point-inverse c
                                             (ecdh-gen-secret c
                                                              my-priv
                                                              partner-pub)))
                  'ElGamalPlaintext)))