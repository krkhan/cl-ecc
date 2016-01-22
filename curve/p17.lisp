;;;; p17.lisp

(in-package #:cl-ecc)

(defvar *p17-gen* (make-instance
                   'Point
                   :x 5
                   :y 1))

(defvar *p17* (make-instance 'Curve
                                   :a 2
                                   :b 2
                                   :p 17
                                   :g *p17-gen*
                                   :n 19))
