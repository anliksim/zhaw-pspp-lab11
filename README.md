# PSPP Lab 11

Functional programming lab using Quicklisp with SBCL compiler.

Note: This lab is using `lisp-devel:latest` instead of `lisp-devel:base` to include quicklisp.

This allows us to import `ql` libs.

```lisp
(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-json")
```


## Run with Docker

Requires a running docker deamon.

To run the exercise files use (might require sudo).

    ./run.sh fp_praktikum_2.lisp