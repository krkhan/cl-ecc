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

(defgeneric ecdsa-gen-pub (curve entropy stream &key)
  (:documentation "Returns: number or hex-string with hex key set to T"))

;; Model
(define-binary-type ecdsa-pub-payload (curve)
  (:reader (in)
           (let ((key-size (* 2 (length (n curve))))
                 (key-point (mul-point curve
                                       (g curve)
                                       (ironclad:octets-to-integer
                                        (read-value 'octet-array in)))))
             (read-value 'octet-array
                         (ironclad:make-octet-input-stream
                          (concatenate '(vector (unsigned-byte 8))
                                       (x key-point)
                                       (y key-point)))
                         :bytes key-size)))
  (:writer (out seq)
           (let ((key-size  (* 2 (length (n curve)))))
             (write-value 'octet-array out seq :bytes key-size))))

(define-binary-type depends-on (slot isclass)
  (:reader (in)
           (let ((sin (ironclad:make-octet-input-stream slot)))
             (read-value isclass sin)))
  (:writer (out value)
           (write-value isclass out value)))

(define-binary-class ECDSA-Keypair ()
  ((private-key octet-array)
   (public-key (depends-on :slot private-key
                           :isclass 'ECDSA-Public-Key))))

(define-binary-class ECDSA-Public-Key ()
  ((version (version-bytes :bytes 1 :number "04"))
   (payload (ecdsa-pub-payload :curve *secp256k1* ))))


;; Printing
(defmethod print-object ((object ECDSA-Keypair) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (private-key public-key) object
      (format stream "~&private-key: ~&~10t~d~&~10t0x~a~&length:~&~10t~d~&public-key: ~a"
              private-key (ironclad:byte-array-to-hex-string private-key) (length private-key)
              public-key))))
(defmethod print-object ((object ECDSA-Public-Key) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (version payload) object
      (format stream "~&version: ~&~10t~d~&~10t0x~a~&length:~&~10t~d~&payload: ~&~10t~d~&~10t0x~a~&length:~&~10t~d~&"
              version (ironclad:byte-array-to-hex-string version) (length version)
              payload (ironclad:byte-array-to-hex-string payload) (length payload)))))


(defmethod print-object ((sig ECDSASig) out)
  (format out "<ECDSA Sig (r:~x, s:~x)>" (r sig) (s sig)))




;; Methods
(defmethod sig-equalp ((s1 ECDSASig) (s2 ECDSASig))
  (and (= (r s1) (r s2))
       (= (s s1) (s s2))))


(defmethod ecdsa-gen-sig ((ec Curve) (msghash integer) (priv integer) (k integer))
  (with-slots-to-integers (ec-n) (n) ec
    (assert (and (< 0 k) (< k ec-n)))
    (let ((rpoint (mul-point ec (g ec) k)))
      (make-instance 'ECDSASig
                     :r (x rpoint)
                     :s (mul-mod (inv-mod k ec-n)
                                 (add-mod msghash
                                          (mul-mod (x rpoint)
                                                   priv
                                                   ec-n)
                                          ec-n)
                                 ec-n)))))

(defmethod ecdsa-verify-sig ((ec Curve) (msghash integer) (sig ECDSASig) (pub Point))
  (with-slots-to-integers (ec-n) (n) ec
    (assert (point-on-curve-p ec pub))
    (let* ((w (inv-mod (s sig) ec-n))
           (u1 (mul-mod w msghash ec-n))
           (u2 (mul-mod w (r sig) ec-n))
           (pt (add-points ec
                          (mul-point ec (g ec) u1)
                          (mul-point ec pub u2))))
      (with-slots-to-integers (pt-x) (x) pt
        (assert (= (mod pt-x ec-n) (r sig)))))))



;; (defmethod ecdsa-gen-pub ((ec Curve) (k string) stream &key)
;;   "Returns: octet-array to stream. Is the public key from private (random) integer"
;;   (format t "yey"))

;; (defmethod ecdsa-gen-pub ((ec Curve) (hex-string string) stream &key (hex nil))
;;   "Returns: hex string. Is the ublic key from private (random) hex-string"
;;   (with-slots-to-integers (ec-n) (n) ec
;;     (let ((k (parse-integer hex-string :radix 16)))
;;       (assert (and (< 0 k) (< k ec-n)))
;;       (let ((key-point (mul-point ec (g ec) k)))
;;         (write-value 'ecdsa-pub-key
;;                      stream
;;                      (concatenate '(vector (unsigned-byte 8))
;;                                   (x key-point)
;;                                   (y key-point))
;;                      :curve ec)))))
