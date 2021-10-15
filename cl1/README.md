from <https://github.com/alezost>

# Common Lisp - SBCL

We use [QuickLisp](https://www.quicklisp.org/) to install the following
CL libraries:

- [Postmodern](https://github.com/marijnh/Postmodern) for PostgreSQL
- [CL-JSON](https://common-lisp.net/project/cl-json/cl-json.html) for JSON

## Initialization

Run `./init.sh`. It installs QuickLisp and CL dependencies. During this
process, the following files are created:

- `./quicklisp.lisp`: it can be removed as it was used only to bootstrap Quicklisp;
- `~/.sbclrc`: small configuration file for SBCL;
- `~/.quicklisp`: the directory with installed CL packages.

## Testing

Run `./cl-test.lisp`. It should either produce a test assertion (exit
status is 1) or runs successfully (exit status is 0).

On the first run, there will be some compilation output (SBCL compiles
.lisp-files of the freshly installed packages).

Also, there will likely be several harmless warnings in the output
because not all Common Lisp libraries are perfect.
