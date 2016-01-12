;;;; cl-ecc.asd

(asdf:defsystem #:cl-ecc
  :version "0.1.0"
  :description "Describe cl-ecc here"
  :author "krkhan"
  :license "Specify license here"
  :depends-on (:ironclad)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
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
                         (:file "ecc"
                                :depends-on ("common"
                                             "math-mod"
                                             "curve"))))
               (:module "curves"
                        :depends-on (src)
                        :components
                        #.(mapcar #'(lambda (p)
                                      (list :file (pathname-name p)))
                                  (uiop:directory-files
                                   (merge-pathnames "curves/"
                                                    *default-pathname-defaults*)
                                   "*.lisp")))))
