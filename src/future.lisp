;;; This file contains the basic facilities for parallelism. There is one
;;; macro and two functinos, being: future, future-p, and realize, which are
;;; described below.

(in-package #:parallel)

;; This macro essentially creates a new future type -- a computation that
;; happens seperately from the rest of the program, and may or may not be
;; finished
(defmacro future (&rest body)
  `(list 'future (make-thread (lambda () ,@body))))

;; Test whether or not a value is a future.
(defun future-p (f)
  (and (consp f)
       (eq 'future (car f))
       (threadp (cadr f))
       (null (cddr f))))

;; Force a future to be evaluated, or just return the original value if it's
;; not a future.
(defun realize (f)
  (if (future-p f)
    (join-thread (second f))
    f))
