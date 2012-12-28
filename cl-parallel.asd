(asdf:defsystem #:cl-parallel
  :description "A fairly simple parallelism library for common lisp"
  :author "Tom Hulihan"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:bordeaux-threads)
  :components ((:file "src/package.lisp")
               (:file "src/future.lisp" :depends-on ("src/package.lisp"))
               (:file "src/list.lisp"   :depends-on ("src/package.lisp"
                                                     "src/future.lisp"))))
