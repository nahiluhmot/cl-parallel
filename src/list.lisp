;;; This file contains the higher-order parallel functions that are applicable
;;; to lists. Although there are quite a few defuns in this file, the only two
;;; functions that are exported are par-map, par-map-chunked, and
;;; par-map-reduce.

(in-package #:parallel)

(defun par-map-reduce (map-fn reduce-fn xs &key (max-threads 4) (from-end nil) (initial-value nil))
  "This function applys a function to each element of a list in parallel, then
   reduces it using the reducing function and initial value"
  (declare (optimize (speed 3)))
  (labels ((recur (y running to-do)
             (cond ((and (null to-do) (null running)) y)
                   ((or (null to-do) (>= (length running) max-threads))
                    (recur (apply reduce-fn 
                                  (if from-end
                                    (list (realize (car (last running))) y)
                                    (list y (realize (car (last running))))))
                           (butlast running)
                           to-do))
                   (t (recur y
                             (cons (future (funcall map-fn (car to-do)))
                                   running)
                             (cdr to-do))))))
    (recur initial-value nil (if from-end (reverse xs) xs))))

(defun par-map (f xs &key (max-threads 4))
  "This function computes a function upon a list in parallel."
  (par-map-reduce f #'cons xs :max-threads max-threads :from-end t))

;; The following few functions (take through flatten) are utilities for
;; chunking and flattening the list.

         ; Take up to n elements from a list.
(labels ((take (n xs)
           (labels ((recur (m ys zs)
                      (if (or (null ys) (= m 0))
                        (reverse zs)
                        (recur (1- m) (cdr ys) (cons (car ys) zs)))))
             (recur n xs nil)))

         ; Drop up to n elements from a list.
         (drop (n xs)
           (if (or (null xs) (= n 0))
             xs
             (drop (1- n) (cdr xs))))

         ; Split a list into chunks.
         (chunk-list (n xs)
           (if (< n (length xs))
             (cons (take n xs) (chunk-list n (drop n xs))))
             (cons xs nil)))

  (defun par-map-chunked (f size xs &key (max-threads 4))
    "Break a list up into `size` chunks, and process those chunks in parallel."
    (mapcan #'identity
            (par-map (lambda (ys) (mapcar (lambda (y) (funcall f y)) ys))
                     (chunk-list size xs)
                     max-threads))))
