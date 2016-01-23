;;;; cl-ecc.test.asd

(asdf:defsystem #:cl-ecc.test
  :depends-on (#:cl-ecc)
  :serial t
  :components ((:module "common"
                        :pathname "test"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "model")))
               (:module "ecc"
                        :pathname "test/ecc"
                        :depends-on (common)
                        :serial t
                        :components
                        ((:file "utils")
                         (:file "model")

                         (:module "math-tests"
                                  :pathname "math"
                                  :components
                                  ((:file "math-mod-test")))
                         (:module "curve-tests"
                                  :pathname "curves"
                                  :components
                                  #.(mapcar #'(lambda (p) (list :file (pathname-name p)))
                                            (uiop:directory-files
                                             (merge-pathnames "test/ecc/curves/"
                                                              *default-pathname-defaults*)
                                             "*-test.lisp")))
                         (:file "ecc-test")))))
