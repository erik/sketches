(define-module (grains)
  #:export (square total)
  #:autoload (srfi srfi-1) (iota))

(define (square n)
  (expt 2 (- n 1)))

(define (total)
  (apply + (map square (cdr (iota 65)))))
