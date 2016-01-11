;;;; secp192r1.lisp

(in-package #:cl-ecc.curve-parameters)

(defvar *secp192r1-gen* (make-instance
                        'Point
                        :x #x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
                        :y #x07192b95ffc8da78631011ed6b24cdd573f977a11e794811))

(defvar *secp192r1-curve*
  (make-instance
    'Curve
    :a 6277101735386680763835789423207666416083908700390324961276
    :b #x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
    :p 6277101735386680763835789423207666416083908700390324961279
    :g *secp192r1-gen*
    :n 6277101735386680763835789423176059013767194773182842284081))

(export '*secp192r1-gen*)
(export '*secp192r1-curve*)