;;;; p17.lisp

(in-package #:cl-ecc.curve-parameters)

(defvar *p17-gen* (make-instance
                   'Point
                   :x 5
                   :y 1))

(defvar *p17-curve* (make-instance 'Curve
                                   :a 2
                                   :b 2
                                   :p 17
                                   :g *p17-gen*
                                   :n 19))

(export '*p17-gen*)
(export '*p17-curve*)
