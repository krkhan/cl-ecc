;;;; cl-ecc.asd

;;;; cl-ecc.asd

(asdf:defsystem #:cl-ecc
  :version "0.1.0"
  :description "Describe cl-ecc here"
  :author "krkhan"
  :license "Specify license here"
  :depends-on (#:ironclad
               #:iterate
               #:com.gigamonkeys.macro-utilities
               #:alexandria)
  :serial t
  :components ((:module "ecc"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "utils")
                         (:file "common" :depends-on ("utils"))
                         (:file "model-macros" :depends-on ("utils"))
                         (:file "model" :depends-on ("common" "model-macros"))
                         (:file "math-mod")
                         (:file "curve" :depends-on ("model"
                                                     "common"
                                                     "math-mod"))
                         (:file "ecdh" :depends-on ("model" "curve" "model-macros"))
                         (:file "elgamal" :depends-on ("model"  "curve" "ecdh"))
                         (:file "ecdsa" :depends-on ("model"
                                                     "curve"
                                                     "math-mod"
                                                     "model-macros"))
                         (:file "curve-parameters" :depends-on ("model"))
                         (:file "printing" :depends-on ("model"))
                         (:file "ecc" :depends-on ("ecdh"
                                                   "elgamal"
                                                   "ecdsa")))))
:description "A library for eliptic curve cryptography.
               NOT TESTET OR SECURE")
