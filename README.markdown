# Wordbank

Wordbank is command line interface for English-Japanese word translation.

## Usage

In a REPL,
``` console
CL-USER> (load "src/wordbank.lisp")
CL-USER> (main)
```

or if you put this repository into `~/quicklisp/local-projects/local`,

```console
CL-USER> (ql:quickload :wordbank)
CL-USER> (main)
```

After the following installation has been done, instead of executing this program from the REPL, you can run it as a script from a shell like this.

```console
$ ./src/wordbank.lisp
```

## Installation

### SBCL

SBCL(Steal Bank Common Lisp) is required and Quicklisp must be installed on your system. Since this program requires the Lisp core file, you need to evaluate the following forms in REPL and get the core file named `sbcl-base.core` for example.

```cl
(sb-ext:save-lisp-and-die "sbcl-base.core" :executable t)
```

Place 'sbcl-base.core' in an appropriate directory and set the path to the environment variable `SBCL_CORE`.

The following setting is also required in `.sbclrc` for SBCL to ignore a shebang line.

```cl
(set-dispatch-macro-character #\# #\!
  (lambda (stream character n)
    (declare (ignore character n))
    (read-line stream nil nil t)
    nil))
```

### wordbank

Clone this repository in an appropriate directory and run `src/wordbank.lisp` as illustrated in 'Usage' section.

```console
$ git clone git@github.com/tanaka-h/wordbank.git
```
