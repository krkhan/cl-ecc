;;;; ecdsa.lisp


(in-package #:cl-ecc)


;; Generics
(defgeneric sig-equalp (sig1 sig2)
  (:documentation "Returns: Predicate. Test if two ECDSA-Signature are equal"))

(defgeneric ecdsa-gen-sig (curve msghash priv int)
  (:documentation "Returns: 'ECDSA-Signature."))

(defgeneric ecdsa-verify-sig (curve msghash priv int)
  (:documentation "Returns: Predicate. Test if signature is valid"))

(defgeneric ecdsa-gen-pub (curve priv &key version-byte)
  (:documentation "Returns: number or hex-string with hex key set to T"))

(defgeneric get-key (spec object)
  (:documentation
   "Get key from object and returns according to spec.
    Arguments for spec: :int, :vector, :hex-string"))

;; Classes
(defclass ECDSA-Signature ()
  ((r :initarg :r
      :initform (error ":r must be initialized")
      :type 'octet-vector)
   (s :initarg :s
      :initform (error ":s must be initialized")
      :type 'octet-vector)))

(defclass ECDSA-Message-Hash ()
  ((hash :initarg :hash
         :initform (error ":hash must be initialized")
         :type 'octet-vector)))

(defclass ECDSA-Public-Key (Point)
  ((version-byte :initarg :version-byte
                 :initform (error ":version-byte must be initialized")
                 :type 'octet-vector)))

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


;; Methods
(defmethod sig-equalp ((sig1 ECDSA-Signature) (sig2 ECDSA-Signature))
  (let ((r1 (get-slot :int 'r sig1))
        (r2 (get-slot :int 'r sig2))
        (s1 (get-slot :int 's sig1))
        (s2 (get-slot :int 's sig2)))
    (and (= r1 r2)
         (= s1 s2))))

(defmethod ecdsa-gen-sig ((ec Curve) (msghash ECDSA-Message-Hash) (priv Private-Key) (k integer))
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

(defmethod ecdsa-gen-pub ((c Curve) (priv Private-Key) &key (version-byte 04))
  "Returns: ECDSA-Public-Key"
  (let ((priv-key (get-slot :int 'key priv))
        (n (get-slot :int 'n c))
        (g (get-slot :point 'g c)))
    (assert (and (< 0 priv-key) (< priv-key n)))
    (let ((pub-key (mul-point c g priv-key)))
      (change-class pub-key
                    'ECDSA-Public-Key
                    :version-byte (ironclad:integer-to-octets version-byte)))))


;; Printing
(defmethod print-object ((sig ECDSA-Signature) stream)
  (print-unreadable-object (sig stream :type t)
    (format stream "~&~t r:~x ~&~t r:~d ~&~t r:~x
                   ~&~t s:~x ~&~t s:~d ~&~t s:~x"
            (get-slot :vector 'r sig) (get-slot :int 'r sig) (get-slot :hex-string 'r sig)
            (get-slot :vector 's sig) (get-slot :int 's sig) (get-slot :hex-string 's sig))))

(defmethod print-object ((msghash ECDSA-Message-Hash) stream)
  (print-unreadable-object (msghash stream :type t)
    (format stream "~&~t hash:~x ~&~t hash:~d ~&~t hash:~x"
            (get-slot :vector 'hash msghash) (get-slot :int 'hash msghash) (get-slot :hex-string 'hash msghash))))

(defmethod print-object ((pubkey ECDSA-Public-Key) stream)
  (print-unreadable-object (pubkey stream :type t)
    (format stream "~&~t version-byte:~x ~&~t version-byte:~d ~&~t version-byte:~x
                   ~&~t x:~x ~&~t x:~d ~&~t x:~x
                   ~&~t y:~x ~&~t y:~d ~&~t y:~x
                   ~&~t key:~x ~&~t key:~d ~&~t key:~x"
            (get-slot :vector 'version-byte pubkey) (get-slot :int 'version-byte pubkey) (get-slot :hex-string 'version-byte pubkey)
            (get-slot :vector 'x pubkey) (get-slot :int 'x pubkey) (get-slot :hex-string 'x pubkey)
            (get-slot :vector 'y pubkey) (get-slot :int 'y pubkey) (get-slot :hex-string 'y pubkey)
            (get-key :vector pubkey) (get-key :int pubkey) (get-key :hex-string pubkey))))
