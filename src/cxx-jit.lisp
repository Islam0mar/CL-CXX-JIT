(in-package :cxx-jit)

(defparameter *cxx-compiler-executable-path* "/usr/bin/g++")
(defparameter *cxx-compiler-flags* "-std=c++17 -Wall -Wextra -I/usr/include/eigen3")
;;; #\/ '/' should be the last char
(defparameter *cxx-compiler-working-directory* (namestring (uiop:temporary-directory)))
(defconstant +cxx-compiler-lib-name+ (intern "plugin"))
(defconstant +cxx-compiler-wrap-cxx-path+ (uiop:merge-pathnames* "./src/wrap-cxx.cpp" (asdf:system-source-directory :cxx-jit)))
;;; TODO: detect compiler then set flags #+#.
;;;              ,but don't how to handle changing cxx-compiler-exe path
;;; change to "-Wl,-undefined,error -Wl,-flat_namespace" for clang++
(defparameter *cxx-compiler-internal-flags* "-shared -fPIC -Wl,--no-undefined -Wl,--no-allow-shlib-undefined")
(defparameter *cxx-compiler-link-libs* "-lm")
;;; list of libs compiled
(defparameter *cxx-compiler-packages* nil)
(defparameter *cxx-compiler-packages-number* 0)
(defparameter *cxx--fun-names* '())
;; alist used to map C++ type name to cffi types
(defparameter *cxx-type-name-to-cffi-type-symbol-alist* '(("const char*" . :string)
                                                          ("char*" . :string)
                                                          ("void" . :void)
                                                          ("char" . :char)
                                                          ("signed char" . :char)
                                                          ("unsigned char" . :uchar)
                                                          ("short" . :short)
                                                          ("short int" . :short)
                                                          ("signed short int" . :short)
                                                          ("short signed int" . :short)
                                                          ("unsigned short" . :ushort)
                                                          ("unsigned short int" . :ushort)
                                                          ("short unsigned int" . :ushort)
                                                          ("int" . :int)
                                                          ("signed" . :int)
                                                          ("signed int" . :int)
                                                          ("unsigned" . :uint)
                                                          ("unsigned int" . :uint)
                                                          ("long" . :long)
                                                          ("long int" . :long)
                                                          ("signed long" . :long)
                                                          ("signed long int" . :long)
                                                          ("long signed int" . :long)
                                                          ("unsigned long" . :ulong)
                                                          ("unsigned long int" . :ulong)
                                                          ("long unsigned int" . :ulong)
                                                          ("long long" . :llong)
                                                          ("long long int" . :llong)
                                                          ("signed long long" . :llong)
                                                          ("signed long long int" . :llong)
                                                          ("unsigned long long" . :ullong)
                                                          ("unsigned long long int" . :ullong)
                                                          ("float" . :float)
                                                          ("double" . :double)
                                                          ("long double" . :long-double)
                                                          ("bool" . :bool)))

(define-condition cxx-compile-error (error)
  ((message
    :initarg :message
    :reader cxx-compile-error-message))
  (:report (lambda (condition stream)
             (format stream "C++ compile error:~%~A"
                     (cxx-compile-error-message condition)))))

;; inline void lisp_error(const char *error)
(cffi:defcallback lisp-error :void ((err :string))
  (format t "Caught error: ~a~%" err))

(cffi:defcstruct meta-data
  (func-ptr :pointer)
  (method-p :bool)
  (arg-types (:pointer :string))
  (types-size :int8))

(defun string-replace-first (str old new)
  (let ((tmp (search old str)))
    (strcat (subseq str 0 tmp)
            new
            (subseq str (+ tmp (length old))))))


(defun symbols-list (arg-types &optional (method-p nil))
  "Return a list of symbols '(V0 V1 V2 V3 ...)
   representing the number of args"
  (let ((lst (if arg-types (loop for i below (length arg-types)
                                 collect (intern (format nil "V~A" i))))))
    (if method-p (push (intern "OBJ") lst))
    lst))

(defun cffi-type (type)
  "Returns cffi-type as a keyword"
  (declare (type string type))
  (let ((type-symbol (cdr (assoc type
                                 *cxx-type-name-to-cffi-type-symbol-alist*
                                 :test #'string-equal))))
    (cond
      (type-symbol type-symbol)
      ((eq #\* (elt type (1- (length type)))) :pointer)
      (t (format t "No Known conversion for type ~S. default to pointer~%" type) :pointer))))

(defun parse-input-args (arg-types)
  "return argument types (with variables if they are inputs) in a proper list"
  (when arg-types
    (mapcan (lambda (arg-type sym)
              (list (cffi-type arg-type) sym))
            arg-types (symbols-list arg-types))))

;; void send_data(MetaData *M)
(cffi:defcallback reg-data :void ((meta-ptr :pointer))
  (cffi:with-foreign-slots ((func-ptr method-p arg-types types-size) meta-ptr (:struct meta-data))
    (let ((name (pop *cxx--fun-names*))
          (args (loop for i below types-size
                      collect (cffi:mem-aref arg-types :string i))))
      (let ((fname (read-from-string name)))
        (unless (string-prefix-p "%" name)
          (export fname))
        (eval `(defun
                   ,fname ,(symbols-list (cdr args) method-p)
                 ;; TODO: add declare type
                 (cffi:foreign-funcall-pointer
                  ,func-ptr
                  nil
                  ,@(append
                     ;; cxx-ptr defined in defclass
                     (when method-p '(:pointer obj))
                     (parse-input-args (cdr args))
                     (list (cffi-type (car args)))))))))))

(defun compile-code (code)
  "compile aync. code string with cxx compiler"
  ;; compiler command
  (let* ((cmd (strcat *cxx-compiler-executable-path*
                      " "
                      *cxx-compiler-internal-flags*
                      " "
                      *cxx-compiler-flags*
                      " "
                      ;;*cxx-compiler-output-path*
                      ;;" "
                      *cxx-compiler-working-directory* (symbol-name +cxx-compiler-lib-name+) ".cpp -o "
                      *cxx-compiler-working-directory* (symbol-name +cxx-compiler-lib-name+) ".so "
                      *cxx-compiler-link-libs*)))

    ;; create cxx file and insert code into it
    (with-open-file (cxx-source-code-file (strcat *cxx-compiler-working-directory*
                                                  (symbol-name +cxx-compiler-lib-name+)
                                                  ".cpp")
                                          :direction :output    ;; Write to disk
                                          :if-exists :supersede ;; Overwrite the file
                                          :if-does-not-exist :create)
      (write-string code cxx-source-code-file))
    ;; compile cxx file
    (print cmd)
    (multiple-value-bind (out errs code)
        (uiop:run-program cmd :output :string
                              :error-output :string
                              :ignore-error-status t)
      (format t "~A~%~A" out errs)
      (when (/= code 0)
        (error 'cxx-compile-error :message errs))
      (= code 0))))

(defun copy-and-load-new-library ()
  "if compilation suceceded copy plugin.so to plugin_x.so
           ,where x = 0,1,...
     then load the library"
  (let* ((n_str (write-to-string
                 (1- (setf *cxx-compiler-packages-number*
                           (1+ *cxx-compiler-packages-number*)))))
         (source (strcat *cxx-compiler-working-directory*
                         (symbol-name +cxx-compiler-lib-name+)
                         ".so"))
         (destination (strcat *cxx-compiler-working-directory*
                              (symbol-name +cxx-compiler-lib-name+)
                              "_" n_str ".so")))
    (uiop:copy-file source destination)
    (push
     (cffi:load-foreign-library destination)
     *cxx-compiler-packages*)))

(defun from (header-names import &rest body)
  "import cxx functions/methods from the header"
  (declare (ignore import))
  ;; 1. create code-block
  (let* ((header (with-output-to-string (stream)
                   (dolist (header-name header-names)
                     (format stream "#include ~A~%"
                             (if (member (char header-name 0) '(#\< #\"))
                                 header-name
                                 (strcat "\"" header-name "\""))))))
         (pack-name (symbol-name (gensym "RegisterPackage")))
         (import-str (with-output-to-string (stream)
                       (dolist (f body)
                         (write-string (if (consp f)
                                           (format nil "~%IMPORT(~A);" (car f))
                                           f)
                                       stream))))
         (cxx-code (strcat header (uiop:read-file-string +cxx-compiler-wrap-cxx-path+)))
         (cxx-code (string-replace-first cxx-code "$" pack-name))
         (cxx-code (string-replace-first cxx-code "// BlaBlaBla;" import-str)))

    ;; 2. compile code
    (when (compile-code cxx-code)
      ;; 3. call c function to register package
      (copy-and-load-new-library)
      (let ((*cxx--fun-names* (mapcan (lambda (elem)
                                        (when (consp elem) (list (cdr elem))))
                                      body)))
        (eval `(cffi:foreign-funcall ,pack-name :pointer (cffi:callback lisp-error)
                                                :pointer (cffi:callback reg-data)))))))
