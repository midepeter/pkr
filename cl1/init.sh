#!/bin/sh
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install :path (merge-pathnames (user-homedir-pathname) \".quicklisp\"))" --quit
sbcl --load "$HOME/.quicklisp/setup.lisp" --eval "(progn (setf ql-util::*do-not-prompt* t) (ql:add-to-init-file))" --quit
sbcl --eval "(ql:quickload '(cl-json postmodern))" --quit
