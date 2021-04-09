(defpackage cxx-jit/test
  (:use :cl
        :rove
        ))
(in-package :cxx-jit/test)

(deftest example-test
  (from '("<string>") 'import '("[](std::string x){return \"Hi, \"+x;}" . "hi"))
  (from '("<cmath>") 'import '("static_cast<double(*)(double)>(std::sin)" . "cpp-sin"))
  (ok (string= (hi "there!") "Hi, there!"))
  (ok (= (cpp-sin 0d0) 0d0))
  (ok (= (cpp-sin (/ pi 2)) 1d0)))

(run-suite *package*)
