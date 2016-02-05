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
(define-composite-integer-reader-functions ECDH-Public-key (key x y))

;; ECDH Methods

(defmethod ecdh-gen-pub ((ec Curve) (privkey ECDH-Private-Key))
  (with-accessors ((pkey key)) privkey
    (with-accessors ((n n) (g g)) ec
      (with-octets-to-integer (pkey n)
        (let* ((point-key (mul-point ec g pkey)))
          (assert (and (< 0 pkey) (< pkey n)))
          (make-instance 'ECDH-Public-Key
                         :x (x point-key)
                         :y (y point-key)))))))

(defmethod ecdh-gen-secret ((ec Curve) (privkey ECDH-Private-Key) (partner-pub ECDH-Public-Key))
  (assert (point-on-curve-p ec partner-pub))
  (let* ((pkey (key privkey))
         (point-secret (mul-point ec partner-pub pkey) ))
    (make-instance 'ECDH-Secret
                   :x (x point-secret)
                   :y (y point-secret))))
