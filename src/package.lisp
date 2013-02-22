(defpackage #:parallel
  (:use #:cl #:bordeaux-threads)
  (:export #:future #:future-p #:future-finished-p #:forget #:realize
           #:realize-if-finished #:par-map #:par-map-chunked #:par-find
           #:par-find-if #:par-calls #:par-map-reduce #:par-some #:par-every))
