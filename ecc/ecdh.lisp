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

;; Reader functions
(define-custom-composite-octet-reader-functions ECDH-Public-key (key x y))

;; ECDH Methods

(defmethod ecdh-gen-pub ((ec Curve) (priv ECDH-Private-Key))
  (let* ((priv-key (key priv :int t))
         (g (g ec))
         (n (n ec :int t))
         (point-key (mul-point ec g priv-key)))
    (assert (and (< 0 priv-key) (< priv-key n)))
    (make-instance 'ECDH-Public-Key
                   :x (x point-key)
                   :y (y point-key))))

(defmethod ecdh-gen-secret ((ec Curve) (my-priv ECDH-Private-Key) (partner-pub ECDH-Public-Key))
  (assert (point-on-curve-p ec partner-pub))
  (let ((point-secret (mul-point ec partner-pub (key my-priv :int t)) ))
    (make-instance 'ECDH-Secret
                   :x (x point-secret)
                   :y (y point-secret))))
