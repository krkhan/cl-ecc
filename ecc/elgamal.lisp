;;;; elgamal.lisp
;;;; https://en.wikipedia.org/wiki/ElGamal_encryption

(in-package #:cl-ecc)

(defclass ElGamalMessage (Point) ())
(defclass ElGamalPlaintext (Point)())
(defclass ElGamalCiphertext (Point) ())

(defgeneric elgamal-encrypt (curve plaintext partner-pub my-priv)
  (:documentation "Returns: 'ElGamalMessage"))
(defgeneric elgamal-decrypt (curve ElGamalMessage my-priv)
  (:documentation "Returns: a 'Point."))

;; Methods

(defmethod elgamal-encrypt ((c Curve) (plaintext ELGamalPlaintext)
                            (partner-pub ECDH-Public-Key) (my-priv Private-Key))

  (assert (point-on-curve-p c plaintext))
  (assert (point-on-curve-p c partner-pub))
  (let ((my-pub (ecdh-gen-pub c my-priv)))
    (make-instance
     'ElGamalMessage
     :x my-pub
     :y (change-class (add-points c
                                  plaintext
                                  (ecdh-gen-secret c my-priv partner-pub))
                      'ElGAmalCiphertext))))

(defmethod elgamal-decrypt ((c Curve) (msg ElGamalMessage) (my-priv Private-Key))
  (let ((partner-pub (get-slot :point 'x msg))
        (ciphertext (get-slot :point 'y msg)))
    (assert (point-on-curve-p c partner-pub))
    (assert (point-on-curve-p c ciphertext))
    (change-class (add-points c
                              ciphertext
                              (point-inverse c
                                             (ecdh-gen-secret c
                                                              my-priv
                                                              partner-pub)))
                  'ElGamalPlaintext)))

;; Printing


(defmethod print-object ((msg ElGamalmessage) stream)
  (print-unreadable-object (msg stream :type t)
    (format stream "~&~t x:~a
                   ~&~t y:~a"
            (slot-value msg 'x)
            (slot-value msg 'y))))
