#!/bin/bash

mkdir -p ECDSA
x=1
while [ $x -le $2 ]
do
    openssl ecparam -genkey -name $1  -noout -out ECDSA/priv-$x.pem
    openssl ec -in ECDSA/priv-$x.pem -outform DER|tail -c +8|head -c 32|xxd -p -c 32 >> ECDSA-pairs.key
    openssl ec -in ECDSA/priv-$x.pem -pubout -outform DER|tail -c 65|xxd -p -c 65 >> ECDSA-pairs.key
  x=$(( $x + 1 ))
done
