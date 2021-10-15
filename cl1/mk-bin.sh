#!/bin/sh

sbcl --load cl-test.lisp \
     --eval "(save-lisp-and-die \"test\"
               :toplevel (lambda () (run-tests t))
               :executable t)"
