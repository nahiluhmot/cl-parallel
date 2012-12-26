(in-package #:parallel)

(defmacro future (&rest body)
  `(list 'future (make-thread (lambda () ,@body))))

(defun future-p (f)
  (and (consp f)
       (eq 'future (car f))
       (threadp (cadr f))
       (null (cddr f))))

(defun realize (f)
  (if (future-p f)
    (join-thread (second f))
    f))
