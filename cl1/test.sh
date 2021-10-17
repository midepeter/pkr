#!/bin/sh

sbcl --disable-debugger --load cl-test.lisp --eval "(run-tests 'trace)" --quit
