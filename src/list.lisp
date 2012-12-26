(in-package #:parallel)

(defun take (n xs)
  (unless (or (null xs) (= n 0))
    (cons (car xs) (take (1- n) (cdr xs)))))

(defun drop (n xs)
  (if (or (null xs) (= n 0))
    xs
    (drop (1- n) (cdr xs))))

(defun split-at (n xs)
  (list (take n xs) (drop n xs)))

(defun chunk-list (n xs)
  (if (< n (length xs))
    (destructuring-bind (as bs) (split-at n xs)
      (cons as (chunk-list n bs)))
    (cons xs nil)))

(defun flatten (xs)
  (cond ((null xs) nil)
        ((atom xs) (list xs))
        (t (mapcan #'flatten xs))))

(defun par-map (f xs &optional (max-threads 4))
  (labels ((recur (done running to-do)
             (cond ((and (null to-do) (null running))
                    (reverse done))
                   ((or (null to-do) (>= (length running) max-threads))
                    (recur (cons (realize (car (last running))) done)
                           (butlast running)
                           to-do))
                   (t (recur done
                             (cons (future (funcall f (car to-do))) running)
                             (cdr to-do))))))
    (recur nil nil xs)))

(defun par-map-chunked (f size xs &optional (max-threads 4))
  (flatten (par-map (lambda (ys) (mapcar (lambda (y) (funcall f y)) ys))
                    (chunk-list size xs)
                    max-threads)))
