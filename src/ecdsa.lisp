;;;; ecdsa.lisp


(in-package #:cl-ecc)

(defclass ECDSASig ()
  ((r :initarg :r :accessor r)
   (s :initarg :s :accessor s)))

(defgeneric sig-equal-p (sig1 sig2)
  (:documentation "Returns: Predicate. Test if two ECDSASig are equal"))

(defgeneric ecdsa-gen-sig (curve msghash priv int)
  (:documentation "Returns: 'ECDSASig."))

(defgeneric ecdsa-verify-sig (curve msghash priv int)
  (:documentation "Returns: Predicate. Test if signature is valid"))


(defmethod sig-equalp ((s1 ECDSASig) (s2 ECDSASig))
  (and (= (r s1) (r s2))
       (= (s s1) (s s2))))

(defmethod print-object ((sig ECDSASig) out)
  (format out "<ECDSA Sig (r:~x, s:~x)>" (r sig) (s sig)))

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
  (let* ((w (inv-mod (s sig) (n c)))
         (u1 (mul-mod w msghash (n c)))
         (u2 (mul-mod w (r sig) (n c)))
         (p (add-points
              c
              (mul-point c (g c) u1)
              (mul-point c pub u2))))
    (assert (= (mod (x p) (n c)) (r sig)))))
