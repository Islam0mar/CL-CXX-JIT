(in-package :cxx-jit)

(defparameter *cxx-compiler-executable-path* "/usr/bin/g++")
(defparameter *cxx-compiler-flags* "-std=c++17 -Wall -Wextra -I/usr/include/eigen3")
;;; #\/ '/' should be the last char
(defparameter *cxx-compiler-working-directory* "/tmp/")
(defconstant +cxx-compiler-lib-name+ (intern "plugin"))
(defconstant +cxx-compiler-wrap-cxx-path+ (uiop:merge-pathnames* "./src/wrap-cxx.cpp" (asdf:system-source-directory :cxx-jit)))
;;; TODO: detect compiler then set flags #+#.
;;;              ,but don't how to handle changing cxx-compiler-exe path
;;; change to "-Wl,-undefined,error -Wl,-flat_namespace" for clang++
(defparameter *cxx-compiler-internal-flags* "-shared -fPIC -Wl,--no-undefined -Wl,--no-allow-shlib-undefined")
(defparameter *cxx-compiler-link-libs* "-lm")
;;; async process value
(defparameter *cxx-compiler-process* nil)
;;; list of libs compiled
(defparameter *cxx-compiler-packages* nil)
(defparameter *cxx-compiler-packages-number* 0)
(defparameter *cxx--fun-names* '())
;; alist used to map C++ type name to cffi types
(defparameter *cxx-type-name-to-cffi-type-symbol-alist* '(("const char*" . :string)
                                                          ("char*" . :string)
                                                          ("void" . :void)
                                                          ("char" . :char)
                                                          ("unsigned char" . :uchar)
                                                          ("short" . :short)
                                                          ("unsigned short" . :ushort)
                                                          ("int" . :int)
                                                          ("unsigned int" . :uint)
                                                          ("long" . :long)
                                                          ("unsigned long" . :ulong)
                                                          ("long long" . :llong)
                                                          ("unsigned long long" . :ullong)
                                                          ("float" . :float)
                                                          ("double" . :double)
                                                          ("long double" . :long-double)
                                                          ("bool" . :bool)))


;; inline void lisp_error(const char *error)
(cffi:defcallback lisp-error :void ((err :string))
  (format t "Caught error: ~a~%" err))

(cffi:defcstruct meta-data
  (func-ptr :pointer)
  (method-p :bool)
  (arg-types (:pointer :string))
  (types-size :int8))


(defun symbols-list (arg-types &optional (method-p nil))
  "Return a list of symbols '(V0 V1 V2 V3 ...)
   representing the number of args"
  (let ((lst (if arg-types (loop for i below (length arg-types)
                                 collect (intern (concatenate 'string "V" (write-to-string i)))))))
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
  (if arg-types (loop
                  for i in arg-types
                  for sym in (symbols-list arg-types)
                  as type = (cffi-type i) then (cffi-type i)
                  append
                  `(,type ,sym))))

;; void send_data(MetaData *M)
(cffi:defcallback reg-data :void ((meta-ptr :pointer))
  (cffi:with-foreign-slots ((func-ptr method-p arg-types types-size) meta-ptr (:struct meta-data))
    (let ((name (pop *cxx--fun-names*))
          (args (loop for i below types-size
                      collect (cffi:mem-aref arg-types :string i))))
      (eval `(progn
               ;; don't export functions starting with '%'
               ,(if (equal #\% (char name 0))
                    nil
                    `(export ',(read-from-string name)))
               (defun
                   ,(read-from-string name) ,(symbols-list (cdr args) method-p)
                 ;; TODO: add declare type
                 (cffi:foreign-funcall-pointer
                  ,func-ptr
                  nil
                  ,@(append
                     (if method-p
                         ;; cxx-ptr defined in defclass
                         (append '(:pointer obj) (parse-input-args (cdr args)))
                         (parse-input-args (cdr args)))
                     (list (cffi-type (car args)))))))))))

(defun compile-code (code)
  "compile aync. code string with cxx compiler"
  ;; compiler command
  (let* ((cmd (concatenate 'string
                           *cxx-compiler-executable-path*
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
    (with-open-file (cxx-source-code-file (concatenate
                                           'string
                                           *cxx-compiler-working-directory*
                                           (symbol-name +cxx-compiler-lib-name+)
                                           ".cpp")
                                          :direction :output    ;; Write to disk
                                          :if-exists :supersede ;; Overwrite the file
                                          :if-does-not-exist :create)
      (format cxx-source-code-file "~A" code))
    ;; compile cxx file
    (print cmd)
    (setf *cxx-compiler-process*
          (uiop:launch-program cmd :output :stream
                                   :error-output :stream))))

(defun try-get-cxx-compiler-output ()
  "returns nil if compiler process is compiling
else returns the exit value from the process"
  (if (uiop/launch-program:process-alive-p *cxx-compiler-process*)
      nil
      (progn
        (loop for line = (read-line
                          (uiop:process-info-error-output
                           *cxx-compiler-process*) nil nil)
              while line
              do (print line) )
        (loop for line = (read-line
                          (uiop:process-info-output
                           *cxx-compiler-process*) nil nil)
              while line
              do (print line) )
        (uiop:wait-process *cxx-compiler-process*))))

(defun copy-and-load-new-library ()
  "if compilation suceceded copy plugin.so to plugin_x.so
           ,where x = 0,1,...
     then load the library"
  (let ((exit-code (try-get-cxx-compiler-output)))
    (when (eq exit-code 0)
      (let* ((n_str (write-to-string
                     (1- (setf *cxx-compiler-packages-number*
                               (1+ *cxx-compiler-packages-number*)))))
             (source (concatenate 'string
                                  *cxx-compiler-working-directory*
                                  (symbol-name +cxx-compiler-lib-name+)
                                  ".so"))
             (destination (concatenate 'string
                                       *cxx-compiler-working-directory*
                                       (symbol-name +cxx-compiler-lib-name+)
                                       "_" n_str ".so")))
        (uiop:copy-file source destination)
        (push
         (eval `(cffi:use-foreign-library ,destination))
         *cxx-compiler-packages*)))))

(defun from (header-names import &rest body)
  "import cxx functions/methods from the header"
  (declare (ignore import))
  ;; 1. create code-block
  (let* ((header
           (format nil "~{~a~}"
                   (loop for header-name in header-names
                         collect
                         (concatenate 'string
                                      "#include "
                                      (if
                                       (or (eq  #\< (aref header-name 0))
                                           (eq  #\" (aref header-name 0)))
                                       header-name
                                       (concatenate 'string
                                                    "\""
                                                    header-name
                                                    "\""))
                                      "
"))))
         (cxx-code (concatenate 'string header (uiop:read-file-string +cxx-compiler-wrap-cxx-path+)))
         (insert-code-pos-str "// BlaBlaBla;")
         (insert-pack-name-pos-str "$")
         (pack-name (symbol-name (gensym "RegisterPackage")))
         (lst '())
         (tmp '())
         (fun-names '())
         (import-str (format nil "~{~a~}"
                             (progn
                               (loop for f in body
                                     do (if (consp f)
                                            (progn
                                              (setf fun-names (append fun-names (list (cdr f))))
                                              (setf lst (append lst
                                                                (list
                                                                 (concatenate 'string
                                                                              "
    IMPORT("
                                                                              (car f) ");")))))
                                            (setf lst (append lst (list f)))))
                               lst))))
    (setf tmp (search insert-pack-name-pos-str cxx-code))
    (setf cxx-code (concatenate 'string
                                (subseq cxx-code 0 tmp)
                                pack-name
                                (subseq cxx-code (+ tmp (length insert-pack-name-pos-str)))))
    (setf tmp (search insert-code-pos-str cxx-code))
    (setf cxx-code (concatenate 'string
                                (subseq cxx-code 0 tmp)
                                import-str
                                (subseq cxx-code (+ tmp (length insert-code-pos-str)))))
    ;; 2. compile code
    (compile-code cxx-code)
    ;; 3. call c function to register package
    (setf *cxx--fun-names* fun-names)
    (loop while (not (try-get-cxx-compiler-output)))
    (copy-and-load-new-library)
    (if (eq (try-get-cxx-compiler-output) 0) (eval
                                              `(cffi:foreign-funcall  ,pack-name :pointer (cffi:callback lisp-error)
                                                                                 :pointer (cffi:callback reg-data))))))
