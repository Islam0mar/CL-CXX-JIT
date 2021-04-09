#|
  This file is a part of lcm project.
  Copyright (c) 2021 Islam Omar (io1131@fayoum.edu.eg)
|#

(defpackage :cxx-jit/test/system
  (:use :cl :asdf))

(in-package :cxx-jit/test/system)

(defsystem :cxx-jit-test
  :author "Islam Omar"
  :license "MIT"
  :depends-on (:cxx-jit
               :rove)
  :components ((:module "tests"
                :components
                ((:file "functions-test"))))
  :description "Test system for cxx-jit"

  :perform (test-op (op c) (symbol-call :rove '#:run c)))
