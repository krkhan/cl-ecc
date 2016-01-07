;;;; cl-ecc.asd

(asdf:defsystem #:cl-ecc
  :version "0.1.0"
  :description "Describe cl-ecc here"
  :author "krkhan"
  :license "Specify license here"
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "ecc"
                                :depends-on ("common"
                                             "math-mod"
                                             "curve"))
                         (:file "utils-clos")
                         (:file "common")
                         (:file "math-mod"
                                :depends-on ("common"))
                         (:file "curve"
                                :depends-on ("common"
                                             "math-mod"
                                             "utils-clos"))
                         (:file "ecdh"
                                :depends-on ("curve"
                                             "math-mod"))
                         (:file "elgamal"
                                :depends-on ("curve"))
                         (:file "ecdsa"
                                :depends-on ("curve"
                                             "math-mod"))
                         (:file "constants"
                                :depends-on ("curve"))))))
