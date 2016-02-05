;;;; utils.lisp
(in-package #:cl-ecc.test)

(defun conc-sym-name (prefix end)
  (let ((prefix-s (string prefix))
        (end-s (string end)))
    (intern (concatenate 'string (string prefix-s) (string end-s)))))

(defun conc-global-sym-name (prefix end)
  (let ((prefix-s (string prefix))
        (end-s (string end)))
    (intern (concatenate 'string "*" (string prefix-s) (string end-s) "*"))))

(defun conc-to-global-sym (sym)
  (let ((sym-s (string sym)))
    (intern (concatenate 'string "*" (string sym-s) "*"))))
