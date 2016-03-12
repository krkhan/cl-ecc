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
(defgeneric ecdsa-ber-encode (signature)
  (:documentation "Generic Function.
                   ARGLIST: 'ECDSA-Signature.
                   RETURNS: '(simple-array '(unsigned-byte (8) (*)).
                   Returns the der encoded signature."))
(defgeneric ecdsa-ber-decode (sequence)
  (:documentation "Generic Function.
                   ARGLIST: 'vector.
                   RETURNS: 'ECDSA-Signature.
                   Returns an 'ECDSA-Signature from a vector sequence."))

;; ECDSA Classes
(defclass ECDSA-Private-Key (Private-Key) ())

(defclass ECDSA-Signature ()
  ((r :initarg :r :reader r :type integer
      :initform (error 'unbound-slot :msg ":r must be initialized"))
   (s :initarg :s :reader s :type integer
      :initform (error 'unbound-slot :msg ":s must be initialized"))))

(defclass ECDSA-Message-Hash ()
  ((hash :initarg :hash :reader hash :type integer
         :initform (error 'unbound-slot :msg ":hash must be initialized"))))

(defclass ECDSA-Public-Key (Public-Key Point)
  ((version-byte :initarg :version-byte :reader version-byte :type integer
                 :initform (error 'unbound-slot :msg ":version-byte must be initialized"))))

;; Key reader function
(defmethod key ((pubkey ECDSA-Public-Key))
  (let ((vb (ironclad:integer-to-octets (version-byte pubkey) :n-bits 8))
        (x (ironclad:integer-to-octets (x pubkey) :n-bits (* 8 (floor (bytes pubkey) 2))))
        (y (ironclad:integer-to-octets (y pubkey) :n-bits (* 8 (floor (bytes pubkey) 2)))))
    (ironclad:octets-to-integer (concatenate 'octet-vector vb x y))))

;; ECDSA methods
(defmethod ecdsa-sig-equalp ((sig1 ECDSA-Signature) (sig2 ECDSA-Signature))
  (with-accessors ((r1 r) (s1 s)) sig1
    (with-accessors ((r2 r) (s2 s)) sig2
          (and (= r1 r2)
               (= s1 s2)))))

(defmethod ecdsa-gen-sig ((ec Curve) (msghash ECDSA-Message-Hash) (priv ECDSA-Private-Key) (k integer))
  (with-accessors ((n n) (g g)) ec
    (let ((hash (hash msghash))
          (key (key priv))
          (n (n ec)))
      (assert (and (< 0 k) (< k n)))
      (let* ((rpoint (mul-point ec (g ec) k))
             (r (x rpoint))
             (s (mul-mod (inv-mod k n) (add-mod hash (mul-mod r key n) n) n)))
        (make-instance 'ECDSA-Signature
                       :r r
                       :s s)))))

(defmethod ecdsa-verify-sig ((ec Curve) (msghash ECDSA-Message-Hash) (sig ECDSA-Signature) (pub ECDSA-Public-Key))
  (assert (point-on-curve-p ec pub))
  (with-accessors ((r r) (s s)) sig
    (with-accessors ((n n) (g g)) ec
      (with-accessors ((hash hash)) msghash
          (let* ((w (inv-mod s n))
                 (u1 (mul-mod w hash n))
                 (u2 (mul-mod w r n))
                 (pt (add-points ec (mul-point ec g u1) (mul-point ec pub u2)))
                 (x (x pt)))
            (assert (= (mod x n) r))
            t)))))

(defun ecdsa-gen-priv ()
  "Function.
   ARGLIST: None.
   RETURNS: 'ECDSA-Private-Key.
   Generates a new random 256 bit private key."
  (make-instance 'ECDSA-Private-key :key (ironclad:octets-to-integer
                                          (ironclad:digest-sequence
                                           :sha256
                                           (ironclad:integer-to-octets (random (expt 2 256)))))))

(defmethod ecdsa-gen-pub ((ec Curve) (priv ECDSA-Private-Key) &key (version-byte 04))
  "Returns: ECDSA-Public-Key"
  (with-accessors ((pkey key)) priv
    (with-accessors ((n n) (g g)) ec
        (let ((point-key (mul-point ec g pkey)))
          (assert (and (< 0 pkey) (< pkey n)))
          (make-instance 'ECDSA-Public-Key
                         :x (x point-key)
                         :y (y point-key)
                         :version-byte version-byte
                         :bytes (+ 1 (* 2 (bytes (g ec)))))))))

(defmethod ecdsa-ber-encode ((sig ECDSA-Signature))
  (let ((r (r sig))
        (s (s sig)))
    (let ((der-vector (asn.1:ber-encode (list r s))))
      (make-array (length der-vector)
                  :element-type '(unsigned-byte 8)
                  :initial-contents (coerce der-vector 'list)))))

(defmethod ecdsa-ber-decode ((ber-seq sequence))
  (destructuring-bind (r s) (coerce (asn.1:ber-decode ber-seq) 'list)
    (make-instance 'ECDSA-Signature
                   :r r
                   :s s)))
