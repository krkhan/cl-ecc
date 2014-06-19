(in-package :ecc)

(defconstant *p17-gen* (make-instance
                              'Point
                              :x 5
                              :y 1))

(defconstant *p17-order* 19)

(defconstant *p17-points* '((5 1)
                            (6 3)
                            (10 6)
                            (3 1)
                            (9 16)
                            (16 13)
                            (0 6)
                            (13 7)
                            (7 6)
                            (7 11)
                            (13 10)
                            (0 11)
                            (16 4)
                            (9 1)
                            (3 16)
                            (10 11)
                            (6 14)
                            (5 16)))

(defconstant *p17-curve*
  (make-instance
    'Curve
    :a 2
    :b 2
    :p 17
    :g (make-instance
         'Point
         :x 5
         :y 1)
    :n 19))

(defconstant *nistp192-gen* (make-instance
                              'Point
                              :x #x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
                              :y #x07192b95ffc8da78631011ed6b24cdd573f977a11e794811))

(defconstant *nistp192-curve*
  (make-instance
    'Curve
    :a 6277101735386680763835789423207666416083908700390324961276
    :b #x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
    :p 6277101735386680763835789423207666416083908700390324961279
    :g *nistp192-gen*
    :n 6277101735386680763835789423176059013767194773182842284081))

(defconstant *nistp192-mulpoints* (make-hash-table))

(setf (gethash '1 *nistp192-mulpoints*)
      (make-instance 'Point
        :x #x188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012
        :y #x07192B95FFC8DA78631011ED6B24CDD573F977A11E794811))
(setf (gethash '2 *nistp192-mulpoints*)
      (make-instance 'Point
        :x #xDAFEBF5828783F2AD35534631588A3F629A70FB16982A888
        :y #xDD6BDA0D993DA0FA46B27BBC141B868F59331AFA5C7E93AB))
(setf (gethash '3 *nistp192-mulpoints*)
      (make-instance 'Point
        :x #x76E32A2557599E6EDCD283201FB2B9AADFD0D359CBB263DA
        :y #x782C37E372BA4520AA62E0FED121D49EF3B543660CFD05FD))
(setf (gethash '112233445566778899 *nistp192-mulpoints*)
      (make-instance 'Point
        :x #x81E6E0F14C9302C8A8DCA8A038B73165E9687D0490CD9F85
        :y #xF58067119EED8579388C4281DC645A27DB7764750E812477))
(setf (gethash '1618292094200346491064154703205151664562462359653015613567 *nistp192-mulpoints*)
      (make-instance 'Point
        :x #x74FEC215F253C6BD845831E059B318C87F727B136A700B91
        :y #x4B702B15B126A703E7A7CEC3E0EC81F8DFCA73A59F5D88B9))
(setf (gethash '6277101735386680763835789423176059013767194773182842284078 *nistp192-mulpoints*)
      (make-instance 'Point
        :x #x76E32A2557599E6EDCD283201FB2B9AADFD0D359CBB263DA
        :y #x87D3C81C8D45BADF559D1F012EDE2B600C4ABC99F302FA02))

(defconstant *nistp192-ecdsa-tests-list* '(
  ("msghash" #x5c8190e87adb631bb5537f3f5f478888bd002ff5
   "d" #x0017899949d02b55f9556846411cc9de512c6f16ecdeb1c4
   "pub-x" #x14f69738599689f5706ab71343becc886ef1569a2d1137fe
   "pub-y" #x0cf5a433909e33217fb4df6b9593f71d43fb1c2a5653b763
   "k" #x0098e9c07e8e6adb97b77d85b0c10a265e11737a89a3e37b
   "r" #xaf1f749e3df6220ff04efd178618a977e0838b1b9dc126e3
   "s" #x8990a04c6cc0ff26264ecf8f7831381a9dbc6e53cc8cc860)
  ("msghash" #x5e97fa0177ffff868cdac356508e22cbe730d2
   "d" #x0064c3a51fb6188170f3cdf12b474a77de4ae0052b84ece8
   "pub-x" #x386afa71afc065019f3d2021ead531ed1d365887122d2d4b
   "pub-y" #xbbfb6e9cdb32c2252015acfb4cfb049b08b4cb279c64928a
   "k" #x00797552b9abf541c886f93556103b817a46c9aa0e2bc86f
   "r" #x337be42eebdcedd97678eeaae9d1b231b740a191a293c22a
   "s" #x9d6766b391e95f649e05442453a85466da29eaa97ddcfc62)
  ("msghash" #x8778cce9e8ad6541b9710e3f7067bc2c9cbc6541
   "d" #x006dfc40880e0c42d2cebcdee255040d18bb99190981f9a9
   "pub-x" #xb11956a29af6984043973e2de46d53d870e04687cae59728
   "pub-y" #xc915f88aa0fa9822762cc4e60df759d189a10c486b901d5e
   "k" #x009e03961f041fa811ff88c4948bc0a6d867a7fef5f39453
   "r" #xc459f7bcea050210e3369ac174ba89c823ca1b0d4c0964f6
   "s" #x04715cba9ba31c4ed9bf0be07d194e2a709294472cc60bdf)))

(defvar *nistp192-ecdsa-tests* '())

(dolist (test *nistp192-ecdsa-tests-list*)
  (let ((entry (make-hash-table :test 'equal)) k v)
    (do ((i 0 (+ i 2)))
        ((>= i (length test)))
        (setf k (nth i test))
        (setf v (nth (1+ i) test))
        (setf (gethash k entry) v))
    (push entry *nistp192-ecdsa-tests*)))
