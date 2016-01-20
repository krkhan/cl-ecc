;;;; package.lisp

(defpackage #:cl-ecc
  (:use #:cl #:iterate)
  (:import-from #:com.gigamonkeys.binary-data
                :read-value
                :write-value
                :read-object
                :write-object
                :define-binary-type
                :define-binary-class)
  (:import-from #:com.gigamonkeys.macro-utilities
                :with-gensyms)
  (:export :ECDSA-Keypair
           :ecdsa-pub-key
           :octet-array
           :get-value
           :read-value
           :write-value
           :ecdh-gen-pub
           :ecdh-gen-secret
           :ElGamalMessage
           :elgamal-encrypt
           :elgamal-decrypt
           :ECDSASig
           :ecdsa-gen-sig
           :ecdsa-verify-sig
           :sig-equalp
           :ecdsa-gen-pub

           ;; curves
           :*p17*
           :*secp256k1*
           :*secp192r1*))
