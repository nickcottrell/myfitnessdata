#!/bin/bash

if [ ! -f buildapp.tgz ]; then 
    wget http://www.xach.com/lisp/buildapp.tgz; 
    tar -zxvf buildapp.tgz;
fi 

pushd buildapp-1.1
make
make install
popd
rm -rf buildapp-1.1

 
