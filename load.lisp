(defun reload () (load "load.lisp"))

(ql:quickload 'bordeaux-threads)

(load "src/package.lisp")
(load "src/future.lisp")
(load "src/list.lisp")

