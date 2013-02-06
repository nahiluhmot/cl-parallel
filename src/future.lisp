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

(defun future-finished-p (f)
  "Return true iff the argument is a live thread"
  (and (future-p f)
       (not (thread-alive-p (second f)))))

(defun realize! (f)
  "Force a future to be evaluated, or return nil if the value is not a future."
  (if (future-p f)
    (join-thread (second f))
    nil))

(defun realize-if-finished (f)
  "If the future is finished, return the value; if the future is still running,
   return the future; if the value is not a future, return it."
  (or (and (future-p f)
           (future-finished-p f)
           (realize! f))
      f))

;; Sets a read-macro (#!) for the realize! function.
(set-dispatch-macro-character #\# #\!
  (lambda (stream subchar arg)
    `(realize! ,(read stream t))))
