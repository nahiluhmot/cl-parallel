;;; This file contains the higher-order parallel functions that are applicable
;;; to lists. Although there are quite a few defuns in this file, the only two
;;; functions that are exported are par-map, par-map-chunked, and
;;; par-map-reduce.

(in-package #:parallel)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names collect `(,name (gensym)))
     ,@body))

(defmacro with-thread-queue (not-done doing max-threads &key (on-done nil) (on-reduce nil) (on-build nil))
  (with-gensyms (to-do running)
    `(let ((,to-do ,not-done)
           (,running ,doing))
      (cond ((and (null ,to-do) (null ,running)) ,on-done)
            ((or (null ,to-do) (>= (length ,running) ,max-threads)) ,on-reduce)
            (t ,on-build)))))

(defun par-map-reduce (map-fn reduce-fn xs &key (max-threads 4) (from-end nil) (initial-value nil))
  "This function applys a function to each element of a list in parallel, then
   reduces it using the reducing function and initial value"
  (declare (optimize (speed 3)))
  (labels ((recur (y running to-do)
             (with-thread-queue to-do running max-threads
               :on-done y
               :on-reduce (recur (apply reduce-fn
                                        (if from-end
                                          (list (realize (car (last running))) y)
                                          (list y (realize (car (last running))))))
                                 (butlast running)
                                 to-do)
               :on-build (recur y
                                (cons (future (funcall map-fn (car to-do)))
                                      running)
                                (cdr to-do)))))
    (recur initial-value nil (if from-end (reverse xs) xs))))

(defun par-map (f xs &key (max-threads 4))
  "This function computes a function upon a list in parallel."
  (par-map-reduce f #'cons xs :max-threads max-threads :from-end t))

(defun par-some (pred xs &key (max-threads 4))
  "Given a predicate and a list, return true if at least one element in the
   list satifies the predicate."
  (labels ((recur (running to-do)
             (with-thread-queue to-do running max-threads
               ; #'realize is mapped across the running threads to ensure there
               ; are no zomibes waiting around after the computation is over.
               :on-reduce (if (realize (car (last running)))
                            (progn (mapcar #'realize running) t)
                            (recur (butlast running) to-do))
               :on-build (recur (cons (future (funcall pred (car to-do))) running)
                                (cdr to-do)))))
    (recur nil xs)))

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
