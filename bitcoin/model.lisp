;;;; model.lisp

(in-package #:cl-ecc)


;; Classes

(define-binary-class Btc-Key ()
  ((private-key (chunk :bytes 32))
   (public-key public-key )
   (pub-key-hash pub-key-hash)))

;; Types

(define-binary-type chunk (bytes)
  (:reader (in)
           (read-u1/le-sequence 'vector in bytes))
  (:writer (out value)
           (write-sequence value out)))

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
               (flatten-vectors value) out))

             (t (write-sequence value out)))))

(define-binary-type pub-key-hash (format)
  (:reader (in)
           (cond

             ((eq format 'public)
              (pubhash->pubhashdata (pubkey->pubkey-hash in 00)))

             (t (pubhash->pubhashdata in))))
  (:writer (out value)
           (cond
             ((eq format 'flat) (write-sequence
                                 (flatten-vectors value) out))
             ((eq format 'b58) (write-sequence
                                (pubkeyhash->b58check-string
                                 (flatten-vectors value)) out))
             (t  (write-sequence value out)))))


;; printing

(defmethod print-object ((object Btc-Key) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (private-key public-key pub-key-hash) object
      (format stream "~&private-key: 0x~x~&public-key: 0x~x~&pub-key-hash: 0x~x~&"
              private-key public-key pub-key-hash))))



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
  (read-value 'blob  (known-privkey-to-stream) :bytes 32)) ;; works

(defun test-priv-write ()
  (let ((sout (ironclad:make-octet-output-stream)))
    (write-value 'blob sout (test-priv-read))
    (ironclad:get-output-stream-octets sout)))

(defun test-pub-read (&key (format 'private))
  (read-value 'public-key (known-privkey-to-stream) :format format))

(defun test-pub-write (&key (format 'flat))
  (let ((sout (ironclad:make-octet-output-stream)))
    (write-value 'public-key sout (test-pub-read) :format format)
    (ironclad:get-output-stream-octets sout)))

(defun test-pub-hash-read (&key format)
  (read-value 'pub-key-hash (known-privkey-to-stream) :format format))

(defun test-pub-hash-write (&key (format 'flat))
  (let ((oout (ironclad:make-octet-output-stream))
        (sout (make-string-output-stream)))
    (if (eq format 'b58)
        (progn
          (write-value 'pub-key-hash sout (test-pub-hash-read) :format format)
          (get-output-stream-string sout))
        (progn
          (write-value 'pub-key-hash oout (test-pub-hash-read) :format format)
          (ironclad:get-output-stream-octets oout)))))

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
