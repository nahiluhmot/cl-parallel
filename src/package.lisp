(defpackage #:parallel
  (:use #:cl #:bordeaux-threads)
  (:export #:future #:future-p #:realize))
