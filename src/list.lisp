;;; This file contains the higher-order parallel functions that are applicable
;;; to lists. Although there are quite a few defuns in this file, the only two
;;; functions that are exported are par-map and par-map-chunked.

(in-package #:parallel)

;; This function computes a function upon a list (or lists) in parallel.
(defun par-map (f xs &optional (max-threads 4))
  (labels ((recur (done running to-do)
             (cond ((and (null to-do) (null running))
                    (reverse done))
                   ((or (null to-do) (>= (length running) max-threads))
                    (recur (cons #!(car (last running)) done)
                           (butlast running)
                           to-do))
                   (t (recur done
                             (cons (future (funcall f (car to-do))) running)
                             (cdr to-do))))))
    (recur nil nil xs)))

;; The following few functions (take through flatten) are utilities for
;; chunking and flattening the list.

         ; Take up to n elements from a list.
(labels ((take (n xs)
           (unless (or (null xs) (= n 0))
             (cons (car xs) (take (1- n) (cdr xs)))))

         ; Drop up to n elements from a list.
         (drop (n xs)
           (if (or (null xs) (= n 0))
             xs
             (drop (1- n) (cdr xs))))

         ; Split a list into chunks.
         (chunk-list (n xs)
           (if (< n (length xs))
             (cons (take n xs) (chunk-list n (drop n xs))))
             (cons xs nil))

         ; Flatten out a chunked list.
         (flatten (xs)
           (cond ((null xs) nil)
                 ((atom xs) (list xs))
                 (t (mapcan #'flatten xs)))))
  ;; Break a list up into `size` chunks, and process those chunks in parallel.
  (defun par-map-chunked (f size xs &optional (max-threads 4))
    (flatten (par-map (lambda (ys) (mapcar (lambda (y) (funcall f y)) ys))
                      (chunk-list size xs)
                      max-threads))))

(defun par-map-reduce (map-fn reduce-fn init-val xs &optional (max-threads 4))
  (labels ((recur (y running to-do)
             (cond ((and (null to-do) (null running)) y)
                   ((or (null to-do) (>= (length running) max-threads))
                    (recur (apply reduce-fn (list y #!(car (last running))))
                           (butlast running)
                           to-do))
                   (t (recur y
                             (cons (future (funcall map-fn (car to-do)))
                                   running)
                             (cdr to-do))))))
    (recur init-val nil xs)))
