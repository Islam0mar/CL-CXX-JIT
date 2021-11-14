(cl:defpackage :cxx-jit
  (:use :cl :cffi :uiop :trivial-garbage)
  ;; Basic types and constructors.
  (:export
   #:*cxx-compiler-executable-path
   #:*cxx-compiler-flags*
   #:*cxx-compiler-working-directory*
   #:+cxx-compiler-lib-name+
   #:+cxx-compiler-wrap-cxx-path+
   #:*cxx-compiler-internal-flags*
   #:*cxx-compiler-link-libs*
   #:*cxx-compiler-process*
   #:*cxx-compiler-packages*
   #:*cxx-compiler-packages-number*
   #:*cxx-type-name-to-cffi-type-symbol-alist*
   #:lisp-error
   #:reg-data
   #:try-get-cxx-compiler-output
   #:from))
