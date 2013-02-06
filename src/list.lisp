;;; This file contains the higher-order parallel functions that are applicable
;;; to lists. Although there are quite a few defuns in this file, the only two
;;; functions that are exported are par-map, par-map-chunked, and
;;; par-map-reduce.

(in-package #:parallel)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names collect `(,name (gensym)))
     ,@body))

(defmacro with-thread-queue (lst (in out) &key (max-threads 4) (sleep-time 0) done down up)
  `(labels ((recur (,in ,out)
             (cond ((null ,in) ,done)
                   ((<= ,max-threads (count-if-not #'future-finished-p ,out))
                    (sleep ,sleep-time) ,down)
                   (t ,up))))
    (recur ,lst nil)))

(defun partition-if (pred xs)
  "Given a predicateand a list, will return a list with the first element being
   all of the elements that satisfy the predicate, and the second element being
   all of the elements that do not satisfy the predicate"
  (labels ((recur (ys true-list false-list)
             (if (null ys)
               `(,true-list ,false-list)
               (if (funcall pred (car ys))
                 (recur (cdr ys) (cons (car ys) true-list) false-list)
                 (recur (cdr ys) true-list (cons (car ys) false-list))))))
    (recur (reverse xs) nil nil)))

(defun par-map (f xs &key (max-threads 4) (sleep-time 0))
  "This function computes a function upon a list in parallel."
  (with-thread-queue xs (in out)
    :max-threads max-threads
    :sleep-time  sleep-time
    :done (reverse (mapcar #'realize! out))
    :down (recur in (mapcar #'realize-if-finished out))
    :up   (recur (cdr in) (cons (future (funcall f (car in))) out))))

(defun par-some (pred xs &key (max-threads 4) (sleep-time 0))
  "Given a predicate and a list, return true if at least one element in the
   list satifies the predicate."
  (with-thread-queue xs (in out)
    :max-threads max-threads
    :sleep-time  sleep-time
    :done (and (member t (mapcar #'realize! out)) t)
    :down (let ((running (remove nil (mapcar #'realize-if-finished out))))
            (or (and (member t running) t)
                (recur in running)))
    :up   (recur (cdr in) (cons (future (funcall pred (car in))) out))))

(defun par-find-if (pred xs &key (max-threads 4) (sleep-time 0) from-end)
  "Given a predicate and a list, will return an element in the list that
   satisfies that predicate. Note that this does not guarantee that it will
   return the first element satisfying the predicate"
  (with-thread-queue (if from-end (reverse xs) xs) (in out)
    :max-threads max-threads
    :sleep-time  sleep-time
    :done (find-if pred (mapcar #'realize! out))
    :down (destructuring-bind (not-done done) (partition-if #'future-p (mapcar #'realize-if-finished out))
            (or (and (some #'identity done)
                     (progn (mapcar #'realize! not-done) t)
                     (find-if #'identity done))
                (recur in not-done)))
    :up   (recur (cdr in)
                 (cons (future (and (funcall pred (car in)) (car in)))
                       out))))

(defun par-map-reduce (map-fn reduce-fn xs &key (max-threads 4) (sleep-time 0) initial-value)
  (labels ((recur (acc running to-do)
             (cond ((and (null to-do) (null running)) acc)
                   ((or (null to-do) (<= max-threads (length running)))
                    (sleep sleep-time)
                    (destructuring-bind (not-done done) (partition-if #'future-p (mapcar #'realize-if-finished running))
                      (recur (reduce reduce-fn done :initial-value acc)
                             not-done
                             to-do)))
                   (t (recur acc
                             (cons (future (funcall map-fn (car to-do))) running)
                             (cdr to-do))))))
    (recur initial-value nil xs)))

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

(defmacro par-calls (&rest calls)
  "Make multiple calls in parallel."
  `(mapcar #'realize!  (list ,@(loop for call in calls collect `(future ,call)))))
