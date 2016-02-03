;;;; ecdsa.lisp


(in-package #:cl-ecc)


;; ECDSA Generics

(defgeneric ecdsa-sig-equalp (sig1 sig2)
  (:documentation "Returns: Predicate. Test if two ECDSA-Signature are equal"))

(defgeneric ecdsa-gen-sig (curve msghash priv int)
  (:documentation "Returns: 'ECDSA-Signature."))

(defgeneric ecdsa-verify-sig (curve msghash priv int)
  (:documentation "Returns: Predicate. Test if signature is valid"))

(defgeneric ecdsa-gen-pub (curve priv &key version-byte)
  (:documentation "Returns: ECDSA-Public-Key"))


;; ECDSA Classes
(defclass ECDSA-Private-Key (Private-Key) ())

(defclass ECDSA-Signature ()
  ((r :initarg :r
      :initform (error 'unbound-slot :msg ":r must be initialized")
      :type 'octet-vector)
   (s :initarg :s
      :initform (error 'unbound-slot :msg ":s must be initialized")
      :type 'octet-vector)))
(define-slot-to-octet-vector-parser ECDSA-Signature r s)
(define-custom-octet-reader-functions ECDSA-Signature r s)

(defclass ECDSA-Message-Hash ()
  ((hash :initarg :hash
         :initform (error 'unbound-slot :msg ":hash must be initialized")
         :type 'octet-vector)))
(define-slot-to-octet-vector-parser ECDSA-Message-Hash hash)
(define-custom-octet-reader-functions ECDSA-Message-hash hash)


(defclass ECDSA-Public-Key (Public-Key Point)
  ((version-byte :initarg :version-byte
                 :initform (error 'unbound-slot :msg  ":version-byte must be initialized")
                 :type 'octet-vector)))
(define-slot-to-octet-vector-parser ECDSA-Public-key version-byte x y)
(define-custom-octet-reader-functions ECDSA-Public-key version-byte)
(define-custom-composite-octet-reader-functions ECDSA-Public-key (key version-byte x y))


;; ECDSA methods

(defmethod ecdsa-sig-equalp ((sig1 ECDSA-Signature) (sig2 ECDSA-Signature))
  (let ((r1 (r sig1 :int t))
        (r2 (r sig2 :int t))
        (s1 (s sig1 :int t))
        (s2 (s sig2 :int t)))
    (and (= r1 r2)
         (= s1 s2))))

(defmethod ecdsa-gen-sig ((ec Curve) (msghash ECDSA-Message-Hash) (priv ECDSA-Private-Key) (k integer))
  (assert (and (< 0 k) (< k (n ec :int t))))
  (let ((rpoint (mul-point ec (g ec) k)))
    (make-instance
      'ECDSA-Signature
      :r (x rpoint)
      :s (ironclad:integer-to-octets
          (mul-mod
           (inv-mod k (n ec :int t))
           (add-mod
            (hash msghash :int t)
            (mul-mod (x rpoint :int t) (key priv :int t) (n ec :int t))
            (n ec :int t))
           (n ec :int t))))))

(defmethod ecdsa-verify-sig ((c Curve) (msghash ECDSA-Message-Hash) (sig ECDSA-Signature) (pub ECDSA-Public-Key))
  (assert (point-on-curve-p c pub))
  (let* ((n (n c :int t))
         (r (r sig :int t))
         (w (inv-mod (s sig :int t) n))
         (u1 (mul-mod w (hash msghash :int t) n))
         (u2 (mul-mod w r n))
         (pt (add-points
              c
              (mul-point c (g c) u1)
              (mul-point c pub u2)))
         (x (x pt :int t)))
    (assert (= (mod x n) r))
    t))

(defmethod ecdsa-gen-pub ((c Curve) (priv ECDSA-Private-Key) &key (version-byte 04))
  "Returns: ECDSA-Public-Key"
  (let* ((priv-key (key priv :int t))
        (n (n c :int t))
        (g (g c))
        (point-key (mul-point c g priv-key)))
    (assert (and (< 0 priv-key) (< priv-key n)))
    (assert (and (= (length (x point-key)) (length (x (g c))))
                (= (length (y point-key)) (length (y (g c))))))
    (make-instance 'ECDSA-Public-Key
                   :x (x point-key)
                   :y (y point-key)
                   :version-byte version-byte)))
