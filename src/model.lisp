;;;; model.lisp
(define-binary-type u4/le ()
  (:reader (in)
           (nibbles:read-ub32/le in))
  (:writer (out value)
           (nibbles:write-ub32/le value out)))

(define-binary-type blob (bytes)
  (:reader (in)
           (read-u1/le-sequence 'vector in bytes))
  (:writer (out value)
           (write-ub1/le-sequence value out)))

(define-binary-type public-key ()
  (:reader (in)
           (list (read-byte in)
                 (read-u1/le-sequence 'vector in 32)
                 (read-u1/le-sequence 'vector in 32)))
  (:writer (out value)
           (write-sequence value out)))

(define-binary-type pub-key-hash ()
   (:reader (in)
            (list (read-byte in)
                  (read-value 'blob in :bytes 20)
                  (read-value 'u4/le in)))
   (:writer (out value)
            (write-sequence value out)))

(define-binary-class Btc-Key ()
  ((private-key (blob :bytes 32))
   (public-key public-key )
   (pub-key-hash pub-key-hash)))

;; methods
(defmethod gen-btc-key2 (stream &key (version 04))
  (let (pkey pubkey keyhash)
    (ironclad:digest-stream :sha256 stream)
    (assert (< (ironclad:octets-to-integer pkey)
               (ironclad:octets-to-integer (n *secp256k1*))))
    (setf pubkey (list version (ecdsa-gen-pub *secp256k1* pkey)))
    (setf keyhash (ironclad:digest-sequence
                   :sha256
                   (ironclad:digest-sequence
                    :ripemd160
                    pubkey)))))



;; printing
(defmethod print-object ((object Btc-Key) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (private-key public-key pub-key-hash) object
      (format stream "~&private-key: 0x~x~&public-key: 0x~x~&pub-key-hash: 0x~x~&"
              private-key public-key pub-key-hash))))


;; helper
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


;;;; testing functions

(defun test-pub-read ()
  (read-value 'public-key (ironclad:make-octet-input-stream (ironclad:integer-to-octets (* 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 (random 425347632526374))))))

(defun test-pub-write ()
  (write-value 'public-key (ironclad:make-octet-output-stream) (test-pub-read)))

(defun test-key-read ()
  (read-value 'Btc-Key (ironclad:make-octet-input-stream (ironclad:hex-string-to-byte-array "ff34532345ffffffffffffffff345654324553434546fffffffffffffffffffffffaaabb5323454323456543fffffffffffffffffffff4789765487654456787654345676543245654fffffffff34564fffffff345645323ffffffff3456543ffffffffffffffffffffffffffffffffffffff3456432456432456432ffff3232ffff2345321fff"))))

(defun test-keygen ()
  (gen-btc-key (ironclad:make-octet-input-stream (ironclad:integer-to-octets (random 345345241425364762545263)))))
