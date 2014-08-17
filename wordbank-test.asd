#|
  This file is a part of wordbank project.
|#

(in-package :cl-user)
(defpackage wordbank-test-asd
  (:use :cl :asdf))
(in-package :wordbank-test-asd)

(defsystem wordbank-test
  :author ""
  :license ""
  :depends-on (:wordbank
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "wordbank"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
