#!/bin/sh
curl -O https://beta.quicklisp.org/quicklisp.lisp
echo "NEXT:"
echo "(quicklisp-quickstart:install)"
echo "(ql:quickload '(\"postmodern\" \"cl-json\"))"
sbcl --load quicklisp.lisp
