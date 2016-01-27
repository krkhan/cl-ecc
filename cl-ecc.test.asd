;;;; cl-ecc.test.asd

(asdf:defsystem #:cl-ecc.test
  :depends-on (#:cl-ecc
               #:ironclad
               #:com.gigamonkeys.test-framework)
  :serial t
  :components ((:module "ecc"
                        :pathname "test/ecc"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "tests" :depends-on ("package"))))))
