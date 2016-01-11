;;;; secp256k1-test.lisp

(in-package #:cl-ecc-test.curve-parameters)


(defparameter secp256k1-mulpoints (make-hash-table))

(setf (gethash '1 secp256k1-mulpoints)
      (make-instance
       'Point
       :x #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
       :y #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8))
(setf (gethash '2 secp256k1-mulpoints)
      (make-instance
       'Point
       :x #xC6047F9441ED7D6D3045406E95C07CD85C778E4B8CEF3CA7ABAC09B95C709EE5
       :y #x1AE168FEA63DC339A3C58419466CEAEEF7F632653266D0E1236431A950CFE52A))
(setf (gethash '3 secp256k1-mulpoints)
      (make-instance
       'Point
       :x #xF9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9
       :y #x388F7B0F632DE8140FE337E62A37F3566500A99934C2231B6CB9FD7584B8E672))
(setf (gethash '112233445566778899 secp256k1-mulpoints)
      (make-instance
       'Point
       :x #xA90CC3D3F3E146DAADFC74CA1372207CB4B725AE708CEF713A98EDD73D99EF29
       :y #x5A79D6B289610C68BC3B47F3D72F9788A26A06868B4D8E433E1E2AD76FB7DC76))
(setf (gethash '1618292094200346491064154703205151664562462359653015613567
               secp256k1-mulpoints)
      (make-instance
       'Point
       :x #xAA3367487281789F25CA918BBD316F73EA0C555A75BED2D1ADB3BD79C08AF33E
       :y #x465F0120126FAEFC89569A10F995D187533F26BDDA5DB1E7B5DC03329F1F581B))
(setf (gethash '6277101735386680763835789423176059013767194773182842284078
               secp256k1-mulpoints)
      (make-instance
       'Point
       :x #x2DB8FF9D9D9C2461C1C31F75A65CE5570770E4345A897094E1100252C223EA80
       :y #x7875509F5A75164E103C016EAA2D23A0C059CF824852EAFBFFA1A0748A6A0548))

(defparameter secp256k1-ecdsa-tests-list '(
  ("msghash" #x5c8190e87adb631bb5537f3f5f478888bd002ff5
   "d" #x0017899949d02b55f9556846411cc9de512c6f16ecdeb1c4
   "pub-x" #x48EDE53A72B46C5873D09296F57916621E0CDD781EBD1CFE3B7D3574AED74306
   "pub-y" #xF72B9CD4E5EC297DFD6F52EB578D84A57A07EA2791092A1F4774DFF0675A6C3B
   "k" #x0098e9c07e8e6adb97b77d85b0c10a265e11737a89a3e37b
   "r" #xE57BB68EB010F30F0D3E0AA1BAAF59D670E6647D30E0C1CB09B9FBBC4FC3F31F
   "s" #x8C301DF474DBCB953094EEDFA34DBC771EAF5B4BDD91B5966620B611B6686B88)
  ("msghash" #x5e97fa0177ffff868cdac356508e22cbe730d2
   "d" #x0064c3a51fb6188170f3cdf12b474a77de4ae0052b84ece8
   "pub-x" #x70DD2A56D3230138E3DD2AB3C54371FE182AA9C78FE3711C48C16599D35A6023
   "pub-y" #x9D5D1BDB58DF18C0C77436D92FAF373321DBEC656061348CE366909A80CEDB61
   "k" #x00797552b9abf541c886f93556103b817a46c9aa0e2bc86f
   "r" #x5069552D2417B5627227E8C0EB9FA49B875C3EE54E75DCBBF3BA29EAD69F4F73
   "s" #x8BB230AEF310DEA0BF1E5BA4642B0CB768A4AEC00CF3ED8FF7BEC0A7A3D0192F)
  ("msghash" #x8778cce9e8ad6541b9710e3f7067bc2c9cbc6541
   "d" #x006dfc40880e0c42d2cebcdee255040d18bb99190981f9a9
   "pub-x" #x5C17791C29C2D83BDA051AC7087C0901860A62E92A7807FE27F55D1BE47663B9
   "pub-y" #xE3CB33302C4EFC164E449B4170816938EABBEE97DC01FBC8A06F104D6C554F53
   "k" #x009e03961f041fa811ff88c4948bc0a6d867a7fef5f39453
   "r" #x9F60154DD2A033D2490FF2F30E33DA6E4DFE4D05A136C4EF5A24F4ED7875C88F
   "s" #x78141EA21F423347664D8413219A8C1B70AFB5CF24B8178DA20ACC9A83EC8438)))

(defparameter secp256k1-ecdsa-tests '())

(dolist (test secp256k1-ecdsa-tests-list)
  (let ((entry (make-hash-table :test 'equal)) k v)
    (do ((i 0 (+ i 2)))
        ((>= i (length test)))
        (setf k (nth i test))
        (setf v (nth (1+ i) test))
        (setf (gethash k entry) v))
    (push entry secp256k1-ecdsa-tests)))

(make-Curve-tests secp256k1)