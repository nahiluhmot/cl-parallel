(asdf:defsystem #:cl-parallel
  :description "A fairly simple parallelism library for common lisp"
  :author "Tom Hulihan"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:bordeaux-threads)
  :components ((:file "src/package")
               (:file "src/future" :depends-on ("src/package"))
               (:file "src/list"   :depends-on ("src/package"
                                                "src/future"))))
