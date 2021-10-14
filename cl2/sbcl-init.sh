#!/bin/sh

# This script assumes SBCL and wget are installed. If not already installed, use your
# package manager to install them.

# Get Quicklisp (Common Lisp package manager). Note that it does not modify any locally
# installed quicklisp directory.

wget -c https://beta.quicklisp.org/quicklisp.lisp

# 1. Install quicklisp.
# 2. Get libraries. Postmodern for postgresql. cl-json for JSON.
# 3. Dump the image to disk, Note that this last step will generate a memory Image of
#    of SBCL with all the dependencies compiled in (on my machine, it is 97MB). This has
#    to be done only once. After that, it provides instant startup to our script.

sbcl --no-userinit --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install :path "/tmp/quicklisp/")' \
     --eval "(ql:quickload '(#:postmodern #:cl-json))" \
     --eval '(save-lisp-and-die #p"sbcl-with-ql-postmodern-cljson.core")'

# We no longer need quicklisp
rm quicklisp.lisp
rm -rf /tmp/quicklisp/
