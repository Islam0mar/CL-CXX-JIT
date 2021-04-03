(defpackage cxx/test
  (:use :cl
        :prove
        ))
(in-package :cxx/test)

(pushnew (merge-pathnames #p"ros/lisp-demo/lib/" (user-homedir-pathname))
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library my-lib
  ;; (:darwin (:or "libbipedal.3.dylib" "libbipedal.dylib"))
  ;; (:unix (:or "libbipedal.so.3" "libbipedal.so"))
  (t (:default "libbipedal")))


(cffi:use-foreign-library my-lib)


(plan 1)

;; start here
(ok (cxx:init))

(defun test ()
  (cxx:add-package "TEST" "TEST")
  (cxx:remove-package "TEST")
  )

(ok (test))

(cffi:close-foreign-library 'my-lib)

(finalize)


