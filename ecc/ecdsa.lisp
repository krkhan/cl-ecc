;;;; ecdsa.lisp


(in-package #:cl-ecc)


;; ECDSA Generics

(defgeneric sig-equalp (sig1 sig2)
  (:documentation "Returns: Predicate. Test if two ECDSA-Signature are equal"))

(defgeneric ecdsa-gen-sig (curve msghash priv int)
  (:documentation "Returns: 'ECDSA-Signature."))

(defgeneric ecdsa-verify-sig (curve msghash priv int)
  (:documentation "Returns: Predicate. Test if signature is valid"))

(defgeneric ecdsa-gen-pub (curve priv &key version-byte)
  (:documentation "Returns: number or hex-string with hex key set to T"))


;; ECDSA Classes

(defclass ECDSA-Signature ()
  ((r :initarg :r
      :initform (error 'unbound-slot :msg ":r must be initialized")
      :type 'octet-vector)
   (s :initarg :s
      :initform (error 'unbound-slot :msg ":s must be initialized")
      :type 'octet-vector)))

(defclass ECDSA-Message-Hash ()
  ((hash :initarg :hash
         :initform (error 'unbound-slot :msg ":hash must be initialized")
         :type 'octet-vector)))

(defclass ECDSA-Public-Key (Point)
  ((version-byte :initarg :version-byte
                 :initform (error 'unbound-slot :msg  ":version-byte must be initialized")
                 :type 'octet-vector)))

(defclass ECDSA-Private-Key (Private-Key) ())

;; Reader functions

(define-generic-reader-functions ECDSA-Signature)
(define-generic-reader-functions ECDSA-Message-Hash)
(define-generic-reader-functions ECDSA-Public-Key)

(defmethod get-key ((spec (eql :vector)) (object ECDSA-Public-Key))
  (concatenate 'octet-vector
               (get-slot :vector 'version-byte object)
               (get-slot :vector 'x object)
               (get-slot :vector 'y object)) )

(defmethod get-key ((spec (eql :int)) (object ECDSA-Public-Key))
  (ironclad:octets-to-integer (get-key :vector object)))

(defmethod get-key ((spec (eql :hex-string)) (object ECDSA-Public-Key))
  (ironclad:byte-array-to-hex-string (get-key :vector object)))


;; ECDSA methods

(defmethod sig-equalp ((sig1 ECDSA-Signature) (sig2 ECDSA-Signature))
  (let ((r1 (get-slot :int 'r sig1))
        (r2 (get-slot :int 'r sig2))
        (s1 (get-slot :int 's sig1))
        (s2 (get-slot :int 's sig2)))
    (and (= r1 r2)
         (= s1 s2))))

(defmethod ecdsa-gen-sig ((ec Curve) (msghash ECDSA-Message-Hash) (priv ECDSA-Private-Key) (k integer))
  (assert (and (< 0 k) (< k (get-slot :int 'n ec))))
  (let ((rpoint (mul-point c (get-slot :point 'g ec) k)))
    (make-instance
      'ECDSA-Signature
      :r (x rpoint)
      :s (mul-mod
           (inv-mod k (get-slot :int 'n c))
           (add-mod
            (get-hash :int msghash)
             (mul-mod (x rpoint) (get-slot :int 'key priv) (get-slot :int 'n c))
             (get-slot :int 'n c))
           (get-slot :int 'n c)))))

(defmethod ecdsa-verify-sig ((c Curve) (msghash ECDSA-Message-Hash) (sig ECDSA-Signature) (pub Point))
  (assert (point-on-curve-p c pub))
  (let* ((n (get-slot :int 'n c))
         (r (get-slot :int 'r sig))
         (w (inv-mod (s sig) n))
         (u1 (mul-mod w (get-slot :int 'hash msghash) n))
         (u2 (mul-mod w r n))
         (pt (add-points
              c
              (mul-point c (get-slot :point 'g c) u1)
              (mul-point c pub u2)))
         (x (get-slot :int 'x pt)))
    (assert (= (mod x n) r))
    t))

(defmethod ecdsa-gen-pub ((c Curve) (priv ECDSA-Private-Key) &key (version-byte 04))
  "Returns: ECDSA-Public-Key"
  (let ((priv-key (get-slot :int 'key priv))
        (n (get-slot :int 'n c))
        (g (get-slot :point 'g c)))
    (assert (and (< 0 priv-key) (< priv-key n)))
    (let ((pub-key (mul-point c g priv-key)))
      (change-class pub-key
                    'ECDSA-Public-Key
                    :version-byte (ironclad:integer-to-octets version-byte)))))
