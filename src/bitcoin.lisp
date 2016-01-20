;;;; bitcoin.lisp

(in-package #:cl-ecc)

(defparameter *priv2* "18E14A7B6A307F426A94F8114701E7C8E774E7F9A47E2C2035DB29A206321725")
(defparameter strm (ironclad:make-octet-output-stream))

(define-constant +base58-alphabet+ "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" "The base58 alphabet")


;; BTC-PUB-KEY TYPES
(define-binary-type version-bytes (bytes number)
  (:reader (in)
           (read-value 'octet-array
                       (ironclad:make-octet-input-stream
                        (ironclad:hex-string-to-byte-array number))
                       :bytes 1))
  (:writer (out value)
           (write-value 'octet-array out value :bytes bytes)))

(define-binary-type btc-pub-payload ()
  (:reader (in)
           (let ((spub (ironclad:make-octet-output-stream)))
             (write-object (read-value 'ECDSA-Public-Key in)
                           spub)
             (read-value 'octet-array
                         (ironclad:make-octet-input-stream
                          (ironclad:digest-sequence
                           :ripemd-160
                           (ironclad:digest-sequence
                            :sha256
                            (ironclad:get-output-stream-octets spub))))
                         :bytes 20)))
  (:writer (out seq)
           (write-value 'octet-array out seq :bytes 20)))

(define-binary-type btc-pub-checksum (version payload)
  (:reader (in)
           (read-value 'octet-array (ironclad:make-octet-input-stream
                                     (ironclad:digest-sequence
                                      :sha256
                                      (ironclad:digest-stream
                                       :sha256
                                       (make-concatenated-stream
                                        (ironclad:make-octet-input-stream version)
                                        (ironclad:make-octet-input-stream payload)))))
                       :bytes 4))
  (:writer (out seq)
           (write-value 'octet-array out seq :bytes 4)))


(defun gen-btc-private-key-from-stream (in &key (curve *SECP256K1*))
  (do ((priv-key (ironclad:digest-stream
                  :sha256
                  in)))
      ((< (ironclad:octets-to-integer priv-key)
          (ironclad:octets-to-integer (n curve)))
       (read-value 'octet-array (ironclad:make-octet-input-stream priv-key)
                   :bytes (length (n curve))))
    (setf priv-key (ironclad:digest-sequence :sha256 priv-key))))


(define-binary-type ecdsa-priv-key (curve bytes)
  (:reader (in)
           (let ((priv-key (read-value 'octet-array in :bytes bytes)))
             (assert (< (ironclad:octets-to-integer priv-key)
                        (ironclad:octets-to-integer (n curve))))
             priv-key))
  (:writer (out value)
           (write-value 'octet-array out value :bytes bytes)))

(define-binary-class Btc-Public-Key ()
  ((version (version-bytes :bytes 1 :number "00"))
   (payload (btc-pub-payload))
   (checksum (btc-pub-checksum :version version
                               :payload payload))))

(defclass Keypair ()
  ((x)
   (y)))

(define-binary-type btc-58Check ()
  (:reader (in)
           (let ((spub (ironclad:make-octet-output-stream)))
             (write-object (read-value 'Btc-Public-key in) spub)
             (let* ((hex-string (ironclad:byte-array-to-hex-string
                                 (ironclad:get-output-stream-octets spub)))
                    (value (parse-integer hex-string :radix 16))
                    (leading-ones (make-base58-prefix hex-string)))
               (do ((num value q)
                    (q 0 )
                    (r 0 )
                    (result '()))
                   ((= num 0) (concatenate 'string leading-ones (coerce result 'string)))
                 (setf (values q r) (floor num 58))
                 (push (char +base58-alphabet+ r) result)))))
  (:writer (out string)
           (write-sequence string out)))

(define-binary-class Btc-Address ()
  ((btc-address btc-58Check)))

(define-binary-class Btc-Keypair (keypair)
  ((btc-priv-key (ecdsa-priv-key :curve *SECP256K1*
                                 :bytes 32))
   (btc-pub-key (depends-on :slot btc-priv-key
                            :isclass 'Btc-Public-Key))))

(define-binary-class Btc-Keypair58 (keypair)
  ((btc-priv-key (ecdsa-priv-key :curve *SECP256K1*
                                 :bytes 32))
   (btc-pub-key (depends-on :slot btc-priv-key
                            :isclass 'Btc-Public-Key))
   (btc-address (depends-on :slot btc-pub-key
                            :isclass 'btc-58Check))))



(defmethod print-object ((object Btc-Keypair58) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (btc-address btc-priv-key btc-pub-key) object
      (format stream "~&btc-address(58Check): ~&~10t~d~&
                      ~&length: ~&~10t~d~&"
              btc-address  (length btc-address))
      (print-object btc-pub-key stream)
      (print-object btc-priv-key stream))))


(defmethod print-object ((object Btc-Keypair) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (btc-priv-key btc-pub-key) object
      (format stream "~&btc-priv-key: ~&~10t~d~&~10t0x~x
                      ~&length: ~&~10t~d~&"
              btc-priv-key (ironclad:byte-array-to-hex-string btc-priv-key) (length btc-priv-key))
      (print-object btc-pub-key stream))))

(defmethod print-object ((object Btc-Public-Key) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (version payload checksum) object
      (format stream "~&version: ~&~10t~d~&~10t0x~x
                      ~&length: ~&~10t~d
                      ~&payload: ~&~10t~a~&~10t0x~x
                      ~&length:~&~10t~d
                      ~&checksum: ~&~10t~d~&~10t0x~x
                      ~&length: ~&~10t~d~&
                      ~&TOTAL: ~&~10t0x~x~&"
              version (ironclad:byte-array-to-hex-string version) (length version)
              payload (ironclad:byte-array-to-hex-string payload) (length payload)
              checksum (ironclad:byte-array-to-hex-string checksum) (length checksum)
              (concatenate 'string
                           (ironclad:byte-array-to-hex-string version)
                           (ironclad:byte-array-to-hex-string payload)
                           (ironclad:byte-array-to-hex-string checksum))))))




(defun hex-string-to-base58Check (hex-string)
  (let ((number (parse-integer hex-string :radix 16))
        (leading-ones (make-base58-prefix hex-string)))
    (do ((num number q)
         (q 0 )
         (r 0 )
         (result '()))
        ((= num 0) (concatenate 'string leading-ones (coerce result 'string)))
      (setf (values q r) (floor num 58))
      (push (char +base58-alphabet+ r) result))))

(defun make-base58-prefix (string)
  (do ((count 1 (1+ count))
       (result '())
       (c (char string 0)))
      ((not (eql c #\0)) (coerce result 'string))
    (setf c (char string count))
    (if (eq (mod count 2) 0)
        (push #\1 result))))

(defun base58Check-to-hex-string (base58-string &key (hex nil))
  (let* ((length (length base58-string))
         (rbase58-str (nreverse base58-string))
         (decoded-key (do ((pos 0 (1+ pos))
                           (c (char  rbase58-str 0))
                           (result '()))
                          ((eq pos length) (reduce #'+ result))
                        (push (* (position c +base58-alphabet+) (expt 58 pos )) result)
                        (if (not (eq pos (- length 1)))
                            (setf c (char rbase58-str (+ pos 1)))))))
    (if (eq hex t)
        (format nil "~x" decoded-key)
        decoded-key)))





;; (defmethod print-object ((object (eql 'version)) stream)
;;   (print-unreadable-object (object stream :type t))
;;       (format stream "~&version: ~&~10t~d
;;                       ~&~10t~ajhvblkhjvnbjkhbn
;;                       ~&length: ~&~10t~d"
;;               object (ironclad:byte-array-to-hex-string object) (length object)))
;; (defmethod print-object ((object (eql 'payload)) stream)
;;   (print-unreadable-object (object stream :type t))
;;       (format stream "~&payload: ~&~10t~d
;;                       ~&~10t~a
;;                       ~&length: ~&~10t~d"
;;               payload (ironclad:byte-array-to-hex-string payload) (length payload)))
;; (defmethod print-object ((object (eql 'checksum)) stream)
;;   (print-unreadable-object (object stream :type t))
;;       (format stream "~&checksum: ~&~10t~d
;;                       ~&~10t~a
;;                       ~&length: ~&~10t~d"
;;               checksum (ironclad:byte-array-to-hex-string checksum) (length checksum)))


(defclass Public-Key ()
  ())

(defclass Private-Key ()
  ())


(define-binary-class Ecdsa-Version-Byte (Version-Byte)
  ((ecv-val u2/le :value "04")))




(defclass Coordinate ()
  ())
(defclass X-Coordinate (Coordinate)
  (x-val '(octet-array :bytes bytes)))
(defclass Y-Coordinate (Coordinate)
  (y-val '(octet-array :bytes bytes)))

(defclass 2D-Point ()
  (x)
  (y))


(defmethod read-value ((object (eql '))) )


;;
(define-binary-class Ecdsa-Public-Key (Public-Key Point Ecdsa-Version-Value)
  (()))

(define-binary-class Ecdsa-Private-Key (Private-Key)
  (value u32/le))

((ecdsa-pub-version-byte (version-byte :bytes 1 :number "04"))
 (ecdsa-pub-payload u64/le :depends-on 'ecdsa-priv-key))
;; split in x y and version bit.

(define-binary-class Ecdsa-Key-Pair (Ecdsa-Public-Key Private-Key)
  ())

(defclass Btc-Version-Byte (Version-Byte)
  ((bv-val u2/le)))

(defclass Checksum ()
  ())

(define-binary-class Bitcoin-Public-Key (Btc-Version-Byte Checksum Ecdsa-Public-Key)
  ((btc-payload (payloadd-bytes :bytes 20 :sha256 1 :ripemd160 1))))

(define-binary-class Bitcoin-Key-Pair (Bitcoin-Public-Key Ecdsa-Private-Key))

(defclass Btc58-Version-Byte (Version-Byte)
  ((58v-val 'varint)))


(define-binary-class Bitcoin-Public-Address (Bitcoin-Public-Key)
  ((btc-address '(B58/be))))

(define-binary-class Bitcoin-Address (Bitcoin-Key-pair)
  ((btc-address '(58/be))))
