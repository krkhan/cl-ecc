;;;; cl-ecc-test.asd

(asdf:defsystem :cl-ecc-test
  :depends-on (:cl-ecc)
  :components ((:module "test"
                        :serial t
                        :components
                        ((:file "test-vectors")
                         (:file "test")))))
