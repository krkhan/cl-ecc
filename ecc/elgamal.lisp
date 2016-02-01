;;;; elgamal.lisp
;;;; https://en.wikipedia.org/wiki/ElGamal_encryption

(in-package #:cl-ecc)

(defclass ElGamalMessage ()
  ((c1 :initarg :c1
       :initform (error "c1 must be initialized"))
   (c2 :initarg :c2
       :initform (error "c2 must be initialized"))))

(define-generic-reader-functions ElGamalmessage)



(defgeneric elgamal-encrypt (curve plaintext partner-pub my-priv)
  (:documentation "Returns: 'ElGamalMessage"))
(defgeneric elgamal-decrypt (curve ElGamalMessage my-priv)
  (:documentation "Returns: a 'Point."))

;; Methods

(defmethod elgamal-encrypt ((c Curve) (plaintext Point)
                            (partner-pub ECDH-Public-Key) (my-priv Private-Key))
  (assert (point-on-curve-p c plaintext))
  (assert (point-on-curve-p c partner-pub))
  (let ((my-pub (ecdh-gen-pub c (get-key :int my-priv))))
    (make-instance
     'ElGamalMessage
     :c1 my-pub
     :c2 (add-points
          c
          plaintext
          (ecdh-gen-secret c (get-key :int my-priv) partner-pub)))))

(defmethod elgamal-decrypt ((c Curve) (m ElGamalMessage) (my-priv Private-Key))
  (assert (point-on-curve-p c (get-c1 :int m)))
  (assert (point-on-curve-p c (get-c2 :int m)))
  (add-points c
              (get-c2 :int m )
              (point-inverse c (ecdh-gen-secret c
                                                (get-key :int my-priv)
                                                (get-c1 :int m )))))
