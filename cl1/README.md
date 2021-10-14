from <https://github.com/alezost>

# Common Lisp - SBCL

Two CL dependencies are required:

* <https://github.com/marijnh/Postmodern> for PostgreSQL
* <https://common-lisp.net/project/cl-json/cl-json.html> for JSON

[QuickLisp](https://www.quicklisp.org/) is used to install CL packages. Basically, what needed is this:

````
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
# The lines below are evaluated in SBCL REPL.
(quicklisp-quickstart:install)
(ql:quickload '("postmodern" "cl-json"))
````

The above commands produce "~/.sbclrc" file and "~/.quicklisp" directory (where all the CL packages are placed).

