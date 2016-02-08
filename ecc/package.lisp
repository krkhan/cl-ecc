;;;; package.lisp

(defpackage #:cl-ecc
  (:use #:cl
        #:iterate
        #:alexandria)

  (:shadow :null)

  (:export
   ;;;; external (to be exported)

   ;; ecc
   :ecdh-gen-pub
   :ecdh-gen-secret
   :elgamal-encrypt
   :elgamal-decrypt
   :ecdsa-gen-sig
   :ecdsa-verify-sig
   :ecdsa-sig-equalp
   :ecdsa-gen-pub

   ;; reader functions
   :x :y :key :version-byte :r :s :a :b :p :g :n :h

   ;; classes
   :ECDH-Private-Key
   :ECDH-Public-Key
   :ECDH-Secret
   :ElGamalMessage
   :ElGamalPlaintext
   :ElGamalCiphertext
   :ECDSA-Signature
   :ECDSA-Message-Hash
   :ECDSA-Private-Key
   :ECDSA-Public-Key

   ;; curve variables
   :*p17*
   :*secp256k1*
   :*secp192r1*
   :*inf-point*))
