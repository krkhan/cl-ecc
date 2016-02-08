;;;; cl-ecc.test.asd

(asdf:defsystem #:cl-ecc.test
  :depends-on (#:cl-ecc
               #:alexandria
               #:iterate
               #:ironclad
               #:lisp-unit2
               #:external-program)
  :serial t
  :components ((:module "core"
                        :pathname "test"
                        :serial t
                        :components
                        ((:file "package")
                         (:module "ecc"
                                  :pathname ""
                                  :serial t
                                  :components
                                  ((:file "utils")
                                   (:file "model")))
                         (:module "suites"
                                  :serial t
                                  :components
                                  ((:file "mod-math-suite")
                                   (:file "init-suite")
                                   (:file "curve-suite")
                                   (:file "print-suite")
                                   (:file "ecdsa-suite")))
                         (:module "test-vectors"
                                  :serial t
                                  :components
                                  ((:file "secp192r1-test")
                                   (:file "secp256k1-test")))
                         (:file "cl-ecc.test" :depends-on (ecc suites test-vectors))))))
