;;;; common.lisp

(in-package #:cl-ecc)


;; Constans

(define-constant +base58-alphabet+ "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  "The base58 alphabet")
(define-constant +ecdsa-version-byte+ 04
  "ECDSA key version byte to be prepended")
(define-constant +pubkey-hash-version-byte+ 00
  "Bitcoin public key hash version byte")
(define-constant +script-hash-version-byte+ 05
  "Bitcoin script hash version byte")
(define-constant +privkey-hash-version-byte+ 128
  "Bitcoin private key hash version byte")

;; Data

(defun write-u1/le-sequence (seq stream)
  (write-sequence seq stream))

(defun read-u1/le-sequence (type stream count)
  (ecase type
    (list  (iterate (for i from 1 to count)
                    (collect (read-byte stream))))
    (vector (let ((seq (make-byte-array count)))
              (read-sequence seq stream)
              seq))))

(defun pubkey->pubkeydata (in)
  (list (read-u1/le-sequence 'vector in 1)
        (read-u1/le-sequence 'vector in 32)
        (read-u1/le-sequence 'vector in 32)))

(defun pubhash->pubhashdata (in)
  (list (read-u1/le-sequence 'vector in 1)
        (read-value 'blob in :bytes 20)
        (read-u1/le-sequence 'vector in 4)))


;; Keys

(defmethod gen-btc-key (stream &key
                                 (ecdsa-version +ecdsa-version-byte+)
                                 (btc-version +pubkey-hash-version-byte+))
  ;; this method will not work
  (let ((pkey (nibbles:make-octet-vector 32))
        pubkey keyhash
        (sin (ironclad:make-octet-output-stream)))

    (setf pkey (read-value 'blob stream :bytes 32))

    (assert (< (ironclad:octets-to-integer pkey)
               (n *SECP256K1*)))

    (setf pubkey privkey->pubkey pkey ecdsa-version)
    (setf keyhash (pubkey->pubkey-hash pubkey btc-version))

    (write-sequence pkey sin)
    (write-sequence pubkey sin)
    (write-sequence keyhash sin)
    (read-value 'Btc-Key sin)))

(defmethod pubkey->pubkey-hash (sin btc-version-byte)
  (let ((pubkey (make-array 65 :element-type '(unsigned-byte 8))))
    (read-sequence pubkey sin)
    (ironclad:make-octet-input-stream
     (concatenate '(vector (unsigned-byte 8))
                  (nibbles:octet-vector btc-version-byte)
                  (ironclad:digest-sequence
                   :ripemd-160
                   (ironclad:digest-sequence
                    :sha256
                    pubkey))))))

(defmethod privkey->pubkey (sin ecdsa-version-byte)
    (let ((privkey (make-array 32 :element-type '(unsigned-byte 8))))
      (read-sequence privkey sin)
      (ironclad:make-octet-input-stream
       (concatenate '(vector (unsigned-byte 8))
                    (nibbles:octet-vector ecdsa-version-byte)
                    (ironclad:integer-to-octets
                     (ecdsa-gen-pub *secp256k1*
                                    (ironclad:byte-array-to-hex-string privkey)))))))

(defmethod pubkeyhash->b58Check-string (hash)
  (let ((as-decimal (ironclad:octets-to-integer hash))
        (as-hex-string (ironclad:byte-array-to-hex-string hash))
        leading-ones)
    ;; (declare (type integer as-decimal)
    ;;          (type string as-hex-string)
    ;;          (type string leading-ones))

    (setf leading-ones (do ((count 1 (1+ count))
                            (result '())
                              (c (char as-hex-string 0)))
                           ((not (equal c #\0)) (coerce result 'string))
                         (setf c (char as-hex-string count))
                         (if (eq (mod count 2) 0)
                             (push #\1 result))))
    (do ((num as-decimal q)
         (q 0)
         (r 0)
         (result '()))
        ((= num 0) (concatenate 'string leading-ones (coerce result 'string)))
      (setf (values q r) (floor num 58))
      (push (char +base58-alphabet+ r) result))))
