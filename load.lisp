(in-package :common-lisp-user)

(let ((files '("package"
               "common"
               "error"
               "mod"
               "point"
               "curve"
               "ecdh"
               "elgamal"
               "ecdsa"
               "test-vectors")))
  (format t "Loading the ECC library... ~%")
  (dolist (file files)
    (let* ((path
        (make-pathname 
          :device (pathname-device *load-truename*)
          :directory (pathname-directory *load-truename*)))
        (module (merge-pathnames file path)))
      (compile-file module :verbose nil)
      (load module))))
