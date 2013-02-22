(asdf:defsystem #:cl-parallel-test
  :depends-on (:cl-parallel :lisp-unit)
  :components ((:file "test/package")
               (:file "test/future-test" :depends-on ("test/package"))))
