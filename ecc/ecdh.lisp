;;;; ecdh.lisp

(in-package #:cl-ecc)

;; ECDH Generics

(defgeneric ecdh-gen-pub (curve priv-key)
  (:documentation "Result: a ECDSA-Public-Key (version-byte + 'Point)."))
(defgeneric ecdh-gen-secret (curve my-priv partner-pub)
  (:documentation "Result: ECDSA-Secret (Point)."))

;; ECDH Classes

(defclass ECDH-Private-Key (Private-key) ())
(defclass ECDH-Public-Key (Public-Key Point) ())
(defclass ECDH-Secret (Point) ())

(defmethod initialize-instance :after ((pubkey ECDH-Public-Key) &rest args)
  (declare (ignore args))
  (setf (slot-value pubkey 'key) (concatenate 'octet-vector
                                              (get-slot :vector 'x pubkey)
                                              (get-slot :vector 'y pubkey)) ))

;; ECDH Methods

(defmethod ecdh-gen-pub ((ec Curve) (priv ECDH-Private-Key))
  (let* ((priv-key (get-slot :int 'key priv))
         (g (get-slot :point 'g ec))
         (n (get-slot :int 'n ec))
         (point-key (mul-point ec g priv-key)))
    (assert (and (< 0 priv-key) (< priv-key n)))
    (make-instance 'ECDH-Public-Key
                   :x (slot-value point-key 'x)
                   :y (slot-value point-key 'y))))

(defmethod ecdh-gen-secret ((ec Curve) (my-priv ECDH-Private-Key) (partner-pub ECDH-Public-Key))
  (assert (point-on-curve-p ec partner-pub))
  (let ((point-secret (mul-point ec partner-pub (get-slot :int 'key my-priv)) ))
    (make-instance 'ECDH-Secret
                   :x (slot-value point-secret 'x)
                   :y (slot-value point-secret 'y))))
