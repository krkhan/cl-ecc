;;;; ecdsa.lisp


(in-package #:cl-ecc)


;; Class
(defclass ECDSASig ()
  ((r :initarg :r :accessor r)
   (s :initarg :s :accessor s)))

;; Printing
(defmethod print-object ((sig ECDSASig) out)
  (format out "<ECDSA Sig (r:~x, s:~x)>" (r sig) (s sig)))


;; Generics
(defgeneric sig-equalp (sig1 sig2)
  (:documentation "Returns: Predicate. Test if two ECDSASig are equal"))

(defgeneric ecdsa-gen-sig (curve msghash priv int)
  (:documentation "Returns: 'ECDSASig."))

(defgeneric ecdsa-verify-sig (curve msghash priv int)
  (:documentation "Returns: Predicate. Test if signature is valid"))

(defgeneric ecdsa-gen-pub (curve priv &key version-byte)
  (:documentation "Returns: number or hex-string with hex key set to T"))


;; Methods

(defmethod as-int (accessorfn (sig ECDSASig))
  (ironclad:octets-to-integer (funcall accessorfn sig)))


(defmethod sig-equalp ((sig1 ECDSASig) (sig2 ECDSASig))
  (let ((r1 (as-int #'r sig1))
        (r2 (as-int #'r sig2))
        (s1 (as-int #'s sig1))
        (s2 (as-int #'s sig2)))
    (and (= r1 r2)
         (= s1 s2))))

(defmethod ecdsa-gen-sig ((c Curve) (msghash integer) (priv integer) (k integer))
  (assert (and (< 0 k) (< k (n c))))
  (let ((rpoint (mul-point c (g c) k)))
    (make-instance
      'ECDSASig
      :r (x rpoint)
      :s (mul-mod
           (inv-mod k (n c))
           (add-mod
             msghash
             (mul-mod (x rpoint) priv (n c))
             (n c))
           (n c)))))

(defmethod ecdsa-verify-sig ((c Curve) (msghash integer) (sig ECDSASig) (pub Point))
  (assert (point-on-curve-p c pub))
  (let* ((n (as-int #'n c))
         (r (as-int #'r sig))
         (w (inv-mod (s sig) (n c)))
         (u1 (mul-mod w msghash n))
         (u2 (mul-mod w r n))
         (pt (add-points
              c
              (mul-point c (g c) u1)
              (mul-point c pub u2)))
         (x (as-int #'x pt)))
    (assert (= (mod x n) r))))

(defmethod ecdsa-gen-pub ((c Curve) (k integer) &key version-byte)
  "Returns: number. Is the public key from private (random) integer"
  (declare (ignorable version-byte))
  (assert (and (< 0 k) (< k (as-int #'n c))))
  (let ((pub-key (point->pubkey (mul-point c (g c) k)) :version-byte version-byte))
        pub-key))

(defmethod ecdsa-gen-pub ((c Curve) (hex-string string) &key version-byte)
  "Returns: number. Is the public key from private (random) hex-string"
  (let ((k (parse-integer hex-string :radix 16))
        (vb (when (stringp version-byte)
              (parse-integer version-byte :radix 16))))
    (declare (ignorable vb))
    (assert (and (< 0 k) (< k (as-int #'n c))))
    (let ((pub-key (point->pubkey (mul-point c (g c) k)) :version-byte vb))
      pub-key)))
