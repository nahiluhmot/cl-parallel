;;; This file contains the higher-order parallel functions that are applicable
;;; to lists. Although there are quite a few defuns in this file, the only two
;;; functions that are exported are par-map, par-map-chunked, and
;;; par-map-reduce.

(in-package #:parallel)

(defmacro with-thread-queue (lst (in out) &key (max-threads 4) (sleep-time 0) done down up)
  `(labels ((recur (,in ,out)
             (cond ((null ,in) ,done)
                   ((<= ,max-threads (count-if-not #'future-finished-p ,out))
                    (sleep ,sleep-time) ,down)
                   (t ,up))))
    (recur ,lst nil)))

(defmacro with-sequential-thread-queue (lst (to-do running) &key (max-threads 4) done down up)
  `(labels ((recur (,to-do ,running)
             (cond ((and (null ,to-do) (null ,running)) ,done)
                   ((or (null ,to-do) (<= ,max-threads (length ,running))) ,down)
                   (t ,up))))
    (recur ,lst nil)))

(defun partition-if (pred xs)
  "Given a predicate and a list, will return a list with the first element
   being all of the elements that satisfy the predicate, and the second
   element being all of the elements that do not satisfy the predicate."
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
    :done (reverse (mapcar #'realize out))
    :down (recur in (mapcar #'realize-if-finished out))
    :up   (recur (cdr in) (cons (future (funcall f (car in))) out))))

(defun par-some (pred xs &key (max-threads 4) (sleep-time 0))
  "Given a predicate and a list, return true if at least one element in the
   list satifies the predicate."
  (with-thread-queue xs (in out)
    :max-threads max-threads
    :sleep-time  sleep-time
    :done (and (member t (mapcar #'realize out)) t)
    :down (let ((running (remove nil (mapcar #'realize-if-finished out))))
            (or (and (member t running) t)
                (recur in running)))
    :up   (recur (cdr in) (cons (future (funcall pred (car in))) out))))

(defun par-every (pred xs &key (max-threads 4) (sleep-time 0))
  "Given a predicate and a list, will determine if every element of the list
   satisfies that predicate."
  (with-thread-queue xs (in out)
    :max-threads max-threads
    :sleep-time  sleep-time
    :done (null (delete-if #'identity (mapcar #'realize out)))
    :down (destructuring-bind (not-done done) (partition-if #'future-p (mapcar #'realize-if-finished out))
            (unless (member nil done)
              (recur in not-done)))
    :up   (recur (cdr in)
                 (cons (future (funcall pred (car in))) out))))

(defun par-find-if (pred xs &key (max-threads 4) (key nil key-p) from-end)
  "Given a predicate and a list, will return the first element in the list that
   satisfies that predicate."
  (with-sequential-thread-queue (if from-end (reverse xs) xs) (to-do running)
    :max-threads max-threads
    :down (let ((done (realize (car (last running)))))
            (or (and done
                     (mapcar #'realize running)
                     (or (and key-p (funcall key done)) done))
                (recur to-do (butlast running))))
    :up (recur (cdr to-do)
               (cons (future (and (funcall pred (car to-do)) (car to-do)))
                     running))))

(defun par-find (item xs &key (max-threads 4) from-end)
  "Given an item and a list, will return that item if it is found, nil
   otherwise."
  (and (par-find-if (lambda (x) (eq x item)) xs
                    :max-threads max-threads
                    :from-end from-end)
       item))

(defun par-position-if (pred xs &key (max-threads 4) from-end)
  (let ((x (if from-end (length xs) 0)))
    (with-sequential-thread-queue (if from-end (reverse xs) xs) (to-do running)
      :max-threads max-threads
      :down (or (and (realize (car (last running))) x)
                (recur to-do (butlast running)))
      :up   (recur (cdr to-do)
                   (cons (future (or (funcall pred (car to-do))
                                     (setf x (if from-end (1- x) (1+ x)))
                                     nil))
                         running)))))

(defun par-map-reduce (map-fn reduce-fn xs &key (max-threads 4) (sleep-time 0) initial-value)
  "Given a mapping function, reducing function, and list, will map the values
   accross the list in parallel, then reduce them in the order that the
   computations finish."
  (labels ((recur (acc to-do running)
             (cond ((and (null to-do) (null running)) acc)
                   ((or (null to-do) (<= max-threads (length running)))
                    (sleep sleep-time)
                    (destructuring-bind (not-done done) (partition-if #'future-p (mapcar #'realize-if-finished running))
                      (recur (reduce reduce-fn done :initial-value acc)
                             to-do
                             not-done)))
                   (t (recur acc
                             (cdr to-do)
                             (cons (future (funcall map-fn (car to-do))) running))))))
    (recur initial-value xs nil)))

;; The following few functions (take through chunk-list) are utilities for
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

  (defun par-map-chunked (f xs &key (chunk-size 1) (max-threads 4) (sleep-time 0))
    "Break a list up into `size` chunks, and process those chunks in parallel."
    (mapcan #'identity
            (par-map (lambda (ys) (mapcar (lambda (y) (funcall f y)) ys))
                     (chunk-list chunk-size xs)
                     :max-threads max-threads
                     :sleep-time  sleep-time))))

(defmacro par-calls (&rest calls)
  "Make multiple calls in parallel."
  `(mapcar #'realize  (list ,@(loop for call in calls collect `(future ,call)))))
