;;; This file contains a high level concurrency api, based on futures and
;;; realizations. A future spawns a new thread, and realize blocks until that
;;; thread is finished running.

(in-package #:parallel)

(defmacro future (&rest body)
  "This macro essentially creates a new future type -- a computation that
  happens seperately from the rest of the program, and may or may not be
  finished"
  `(list 'future (make-thread (lambda () ,@body))))

(defun future-p (f)
  "Test whether or not a value is a future."
  (and (consp f)
       (eq 'future (car f))
       (threadp (cadr f))
       (null (cddr f))))

(defun realize (f)
  "Force a future to be evaluated, or just return the original value if it's
  not a future."
  (if (future-p f)
    (join-thread (second f))
    f))

;; Sets a read-macro (#!) for the realize function.
(set-dispatch-macro-character #\# #\!
  (lambda (stream subchar arg)
    `(realize ,(read stream t))))

(defmacro par-calls (&rest calls)
  "Make multiple calls in parallel."
  `(mapcar #'realize  (list ,@(loop for call in calls collect `(future ,call)))))
