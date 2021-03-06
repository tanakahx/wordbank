#|
  This file is a part of wordbank project.
|#

(in-package :cl-user)
(defpackage wordbank-asd
  (:use :cl :asdf))
(in-package :wordbank-asd)

(defsystem wordbank
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:drakma :cl-ppcre :cl-fad)
  :components ((:module "src"
                :components
                ((:file "wordbank"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op wordbank-test))))
