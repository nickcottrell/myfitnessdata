#!/bin/bash
set -e

if [ ! -f buildapp.tgz ]; then
    wget http://www.xach.com/lisp/buildapp.tgz;
    tar -zxvf buildapp.tgz;
fi

pushd buildapp-*.*.*
make
sudo make install
popd
rm -rf buildapp-*.*.*



