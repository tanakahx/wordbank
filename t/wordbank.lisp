#|
  This file is a part of wordbank project.
|#

(in-package :cl-user)
(defpackage wordbank-test
  (:use :cl
        :wordbank
        :cl-test-more))
(in-package :wordbank-test)

;; NOTE: To run this test file, execute `(asdf:test-system :wordbank)' in your Lisp.

(plan nil)

(finalize)
