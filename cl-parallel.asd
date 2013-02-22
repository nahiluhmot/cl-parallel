(asdf:defsystem #:cl-parallel
  :description "A fairly simple parallelism library for Common Lisp"
  :author "Tom Hulihan"
  :license "MIT"
  :version "0.1.2"
  :serial t
  :depends-on (:bordeaux-threads)
  :components ((:file "src/package")
               (:file "src/future" :depends-on ("src/package"))
               (:file "src/list"   :depends-on ("src/future"))))
