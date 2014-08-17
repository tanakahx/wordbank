# Wordbank

Wordbank is command line interface for English-Japanese word translation.

## Usage

If you clone and put this repository into `~/quicklisp/local-projects/local`, you can load with Quicklisp and run the program in a REPL like this. (It is enough to execute `(ql:register-local-projects)` one time, because it registers local projects into `~/quicklisp/local-projects/local/system-index.txt` in order for Quicklisp to find a project next time.)

```console
CL-USER> (ql:register-local-projects)
CL-USER> (ql:quickload "wordbank")
CL-USER> (wordbank:main)
```

After the following installation has been done, instead of executing this program from a REPL, you can run it as a script from a shell like this.

```console
$ ./src/wordbank.lisp
```

## Installation

### SBCL

SBCL (Steel Bank Common Lisp) is required and Quicklisp must be installed on your system. Since this program requires a Lisp core file, you need to evaluate the following forms in a REPL and get a Lisp core file named `sbcl-base.core` for example.

```cl
(sb-ext:save-lisp-and-die "sbcl-base.core" :executable t)
```

Put `sbcl-base.core` in an appropriate directory and set an environment variable `SBCL_CORE`, which holds the path to the Lisp core file.

The following setting is also required in `.sbclrc` in order for SBCL to ignore a shebang line.

```cl
(set-dispatch-macro-character #\# #\!
  (lambda (stream character n)
    (declare (ignore character n))
    (read-line stream nil nil t)
    nil))
```

### Wordbank

Clone this repository in an appropriate directory and run `src/wordbank.lisp` as illustrated in the 'Usage' section.

```console
$ git clone git@github.com:tanaka-h/wordbank.git
```

## License

Copyright (c) 2014 Hiroyuki Tanaka <h.tanaka.mail@gmail.com>

This software is distributed under the MIT license.
