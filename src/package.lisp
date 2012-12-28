(defpackage #:parallel
  (:use #:cl #:bordeaux-threads)
  (:export #:future #:future-p #:realize #:par-map #:par-map-chunked #:par-calls))
