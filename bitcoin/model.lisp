;;;; model.lisp

(in-package #:cl-ecc)


;;;; model


(defun write-u1/le-sequence (seq stream)
  (write-sequence seq stream))

(defun read-u1/le-sequence (type stream count)
  (ecase type
    (list  (iterate (for i from 1 to count)
                    (collect (read-byte stream))))
    (vector (let ((seq (make-array count :element-type '(unsigned-byte 8)
                                         :initial-element 0)))
              (read-sequence seq stream)
              seq))))

(define-binary-type u4/le ()
  (:reader (in)
           (nibbles:read-ub32/le in))
  (:writer (out value)
           (nibbles:write-ub32/le value out)))

(define-binary-type blob (bytes)
  (:reader (in)
           (read-u1/le-sequence 'vector in bytes))
  (:writer (out value)
           (write-sequence value out)))


(defun pubkey->pubkeydata (in)
  (list (read-u1/le-sequence 'vector in 1)
        (read-u1/le-sequence 'vector in 32)
        (read-u1/le-sequence 'vector in 32)))

(define-binary-type public-key (format)
  (:reader (in)
           (cond

             ((eq format 'private)
              (pubkey->pubkeydata (privkey->pubkey in 04)))

             (t (pubkey->pubkeydata in))))

  (:writer (out value)
           (cond

             ((eq format 'flat)
              (write-sequence
               (helper-library:flatten-vectors value) out))

             (t (write-sequence value out)))))

(define-binary-type pub-key-hash (format)
   (:reader (in)
            (list (read-u1/le-sequence 'vector in 1)
                  (read-value 'blob in :bytes 20)
                  (read-u1/le-sequence 'vector in 4)))
   (:writer (out value)
            (cond
              ((eq format 'flat) (write-sequence
                                  (helper-library:flatten-vectors value) out))
              ((eq format 'b58) (write-sequence
                                 (pubkeyhash->b58check-string
                                  (helper-library:flatten-vectors value)) out))
              (t  (write-sequence value out)))))


(define-binary-class Btc-Key ()
  ((private-key (blob :bytes 32))
   (public-key public-key )
   (pub-key-hash pub-key-hash)))





;; methods
(defmethod gen-btc-key (stream &key (ecdsa-version 04) (btc-version 00)) ;; will not work
  (let ((pkey (nibbles:make-octet-vector 32))
        pubkey keyhash
        (sin (ironclad:make-octet-output-stream)))

    (setf pkey (read-value 'blob stream :bytes 32))

    (assert (< (ironclad:octets-to-integer pkey)
               (n *SECP256K1*)))

    (setf pubkey privkey->pubkey pkey ecdsa-version)
    (setf keyhash (pubkey->pubkey-hash pubkey))

    (write-sequence pkey sin)
    (write-sequence pubkey sin)
    (write-sequence keyhash sin)
    (read-value 'Btc-Key sin)))


(defmethod pubkey->pubkey-hash (sin btc-version-byte)
  (let ((pubkey (make-array 65 :element-type '(unsigned-byte 8))))
    (read-sequence hash sin)
    (ironclad:make-octet-input-stream
     (concatenate '(vector (unsigned-byte 8))
                  (nibbles:octet-vector btc-version-byte)
                  (ironclad:digest-sequence
                   :sha256
                   (ironclad:digest-sequence
                    :ripemd-160
                    pubkey))))))

(defmethod privkey->pubkey (sin ecdsa-version-byte)
    (let ((privkey (make-array 32 :element-type '(unsigned-byte 8))))
      (read-sequence privkey sin)
      (ironclad:make-octet-input-stream
       (concatenate '(vector (unsigned-byte 8))
                    (nibbles:octet-vector ecdsa-version-byte)
                    (ironclad:integer-to-octets
                     (ecdsa-gen-pub *secp256k1*
                                    (ironclad:octets-to-integer privkey)))))))

(define-constant +base58-alphabet+ "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" "The base58 alphabet")

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




;; printing
(defmethod print-object ((object Btc-Key) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (private-key public-key pub-key-hash) object
      (format stream "~&private-key: 0x~x~&public-key: 0x~x~&pub-key-hash: 0x~x~&"
              private-key public-key pub-key-hash))))


;;;; testing functions


;; Tests
(defun gen-entropy-to-octet-stream ()
  (ironclad:make-octet-input-stream
   (concatenate 'octet-vector
                (ironclad:digest-sequence
                 :sha256
                 (ironclad:hex-string-to-byte-array "135323267876543aafffffff"))
                (ironclad:digest-sequence
                 :sha256
                 (ironclad:hex-string-to-byte-array "13567876543aafffabb232ffff"))
                (ironclad:digest-sequence
                 :sha256
                 (ironclad:hex-string-to-byte-array "13564344427876543aafffffff")))))
(defun known-privkey-to-stream ()
  (ironclad:make-octet-input-stream
   (ironclad:hex-string-to-byte-array
    "18E14A7B6A307F426A94F8114701E7C8E774E7F9A47E2C2035DB29A206321725")))
(defun known-privkey-to-octet ()
  (ironclad:hex-string-to-byte-array
   "18E14A7B6A307F426A94F8114701E7C8E774E7F9A47E2C2035DB29A206321725"))


;;;;;; ROUND 1 TESTS
(defun test-priv-read ()
  (read-value 'blob  (known-privkey) :bytes 32)) ;; works

(defun test-priv-write ()
  (let ((sout (ironclad:make-octet-output-stream)))
    (write-value 'blob sout (test-priv-read))
    (ironclad:get-output-stream-octets sout)))

(defun test-pub-read (&key (format 'private))
  (read-value 'public-key (known-privkey) :format format))

(defun test-pub-write (&key (format 'flat))
  (let ((sout (ironclad:make-octet-output-stream)))
    (write-value 'public-key sout (test-pub-read) :format format)
    (ironclad:get-output-stream-octets sout)))

(defun test-pub-hash-read (&key format)
  (read-value 'pub-key-hash (known-privkey) :format format))

(defun test-pub-hash-write (&key (format 'flat))
  (let ((oout (ironclad:make-octet-output-stream))
        (sout (make-string-output-stream)))
    (if (eq format 'b58)
        (progn
          (write-value 'pub-key-hash sout (test-pub-hash-read) :format format)
          (get-output-stream-string sout))
        (progn
          (write-value 'pub-key-hash sout (test-pub-hash-read) :format format)
          (ironclad:get-output-stream-octets oout)))))
(defun test-genkey-read ()
  (gen-btc-key (known-privkey)))
(defun test-round1 ()
  (progn
    (test-priv-read)
    (test-priv-write)
    (test-pub-read)
    (test-pub-write :format 'b58t)
    (test-pub-hash-read)
    (test-pub-hash-write :format 'b58)
    (test-genkey-read)))

;;;;;;;
