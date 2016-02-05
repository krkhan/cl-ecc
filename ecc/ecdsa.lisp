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
  ((r :initarg :r :reader r :type octet-vector
      :initform (error 'unbound-slot :msg ":r must be initialized"))
   (s :initarg :s :reader s :type octet-vector
      :initform (error 'unbound-slot :msg ":s must be initialized"))))
(define-slot-to-octet-vector-parser ECDSA-Signature r s)

(defclass ECDSA-Message-Hash ()
  ((hash :initarg :hash :reader hash :type octet-vector
         :initform (error 'unbound-slot :msg ":hash must be initialized"))))
(define-slot-to-octet-vector-parser ECDSA-Message-Hash hash)

(defclass ECDSA-Public-Key (Public-Key Point)
  ((version-byte :initarg :version-byte :reader version-byte :type octet-vector
                 :initform (error 'unbound-slot :msg ":version-byte must be initialized"))))
(define-slot-to-octet-vector-parser ECDSA-Public-key version-byte x y)
(define-composite-octet-reader-functions ECDSA-Public-key (key version-byte x y))

;; ECDSA methods
(defmethod ecdsa-sig-equalp ((sig1 ECDSA-Signature) (sig2 ECDSA-Signature))
  (with-accessors ((r1 r) (s1 s)) sig1
    (with-accessors ((r2 r) (s2 s)) sig2
      (with-octets-to-integer (r1 r2 s1 s2)
          (and (= r1 r2)
               (= s1 s2))))))

(defmethod ecdsa-gen-sig ((ec Curve) (msghash ECDSA-Message-Hash) (priv ECDSA-Private-Key) (k integer))
  (with-accessors ((n n) (g g)) ec
    (let ((hash (hash msghash))
          (key (key priv))
          (n (ironclad:octets-to-integer (n ec))))
      (assert (and (< 0 k) (< k n)))
      (let* ((rpoint (mul-point ec (g ec) k))
             (x (x rpoint)))
        (make-instance 'ECDSA-Signature
                       :r (x rpoint)
                       :s (with-octets-to-integer (hash key x)
                            (ironclad:integer-to-octets
                             (mul-mod (inv-mod k n) (add-mod hash (mul-mod x key n) n) n))))))))

(defmethod ecdsa-verify-sig ((ec Curve) (msghash ECDSA-Message-Hash) (sig ECDSA-Signature) (pub ECDSA-Public-Key))
  (assert (point-on-curve-p ec pub))
  (with-accessors ((r r) (s s)) sig
    (with-accessors ((n n) (g g)) ec
      (with-accessors ((hash hash)) msghash
        (with-octets-to-integer (r s n hash)
          (let* ((w (inv-mod s n))
                 (u1 (mul-mod w hash n))
                 (u2 (mul-mod w r n))
                 (pt (add-points ec (mul-point ec g u1) (mul-point ec pub u2)))
                 (x (ironclad:octets-to-integer (x pt))))
            (assert (= (mod x n) r))
            t))))))

(defmethod ecdsa-gen-pub ((ec Curve) (priv ECDSA-Private-Key) &key (version-byte 04))
  "Returns: ECDSA-Public-Key"
  (with-accessors ((pkey key)) priv
    (with-accessors ((n n) (g g)) ec
      (with-octets-to-integer (pkey n)
        (let ((point-key (mul-point ec g pkey)))
          (assert (and (< 0 pkey) (< pkey n)))
          (assert (and (= (length (x point-key)) (length (x g)))
                       (= (length (y point-key)) (length (y g)))))
          (make-instance 'ECDSA-Public-Key
                         :x (x point-key)
                         :y (y point-key)
                         :version-byte version-byte))))))
