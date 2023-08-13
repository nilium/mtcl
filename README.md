mtcl
====

mtcl is a small Tcl-like interpreter written primarily limited DSL use. It
is not highly optimized, as the interpreter is just AST-walking for the time
being. Currently, the language can be used for a handful of basic things (e.g.,
arithmetic functions are not built-in yet and only integers are supported for
numerics right now), and is made specifically to ensure that more advanced
features (such as user-defined functions, variables, and control flow like
`if` and `foreach`) can be removed or replaced as needed for a given purpose.

Stability
---------

This project is not considered stable and if you end up using it for anything,
it is highly recommended you pin your use to a particular commit. While
things are early I'm going to frequently make breaking changes that
could inconvenience a regular user. Commit history is not guaranteed to
be organized during early commits either. mtcl is still very much in the
hacking-on-it phase.

License
-------

mtcl is licensed under the 2-Clause BSD license (SPDX BSD-2-Clause). A
copy of this license is included with the source code under
[LICENSE.txt](./LICENSE.txt).
