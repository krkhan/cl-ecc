;;;; cl-ecc-test.asd

(asdf:defsystem #:cl-ecc-test
  :depends-on ("cl-ecc")
  :serial t
  :components ((:module "misc-tests"
                        :pathname "test"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "common-test")
                         (:file "math-mod-test"
                                :depends-on ("common-test"))))
               (:module "curve-tests"
                        :pathname "test/curves"
                        :depends-on (misc-tests)
                        :components
                        #.(mapcar #'(lambda (p) (list :file (pathname-name p)))
                                  (uiop:directory-files (merge-pathnames "test/curves/" *default-pathname-defaults*)
                                                        "*-test.lisp")))
               (:module "run"
                        :pathname "test"
                        :components ((:file "cl-ecc-test")))))
