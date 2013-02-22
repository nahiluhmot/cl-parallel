(in-package #:parallel-test)

(define-test future-test
  (assert-equal 'future (car (future 1)))
  (assert-equal 1 (realize (future 1))))

(define-test future-p-test
  (assert-equal t (future-p (future 1)))
  (assert-equal nil (future-p 1)))

(define-test future-finished-p-test
  (assert-equal t  (future-finished-p (future 1)))
  (assert-equal nil (future-finished-p (future (sleep 1) t))))

(define-test forget-test
  (assert-equal nil (forget (future 4)))
  (assert-equal nil (forget 1)))

(define-test realize-if-finished-test
  (assert-equal t (future-p (realize-if-finished (future (sleep 1) 1))))
  (assert-equal nil (future-p (realize-if-finished (let ((x (future 1))) (sleep 1) x)))))
                
