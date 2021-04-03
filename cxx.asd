#|
  This file is a part of cl-cxx project.
  Copyright (c) 2018 Islam Omar (io1131@fayoum.edu.eg)
|#


(defpackage :cxx/system
  (:use :cl :asdf))

(in-package :cxx/system)

(defsystem :cxx
  :version "1.0"
  :author "Islam Omar"
  :license "MIT"
  :depends-on (:cffi :trivial-garbage)
  :components ((:file "package")
               (:module "src"
                        :components ((:file "utilities")
                                     (:file "c-types")
                                     (:file "cxx"))))
  :description "Common Lisp Cxx Interoperation"
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
  :in-order-to ((test-op (test-op "cxx-test"))))
