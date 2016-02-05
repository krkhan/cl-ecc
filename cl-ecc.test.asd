;;;; cl-ecc.test.asd

(asdf:defsystem #:cl-ecc.test
  :depends-on (#:cl-ecc
               #:alexandria
               #:iterate
               #:ironclad
               #:com.gigamonkeys.test-framework
               #:lisp-unit2)
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
                                   (:file "print-suite")))
                         (:module "test-vectors"
                                  :serial t
                                  :components
                                  ((:file "secp192r1-test")
                                   (:file "secp256k1-test")))
                         (:file "cl-ecc.test" :depends-on (ecc suites test-vectors))))))
