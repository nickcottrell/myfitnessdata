#!/bin/sh
set -e

curl -O http://beta.quicklisp.org/quicklisp.lisp
clisp install_quicklisp.lisp
