(define-module (squares)
  #:export (sum-of-squares
            square-of-sums
            difference)
  #:autoload (srfi srfi-1) (reduce iota))


(define (sum-of-squares n)
  (reduce + 0 (map (lambda (x) (expt x 2)) (iota (1+ n)))))

(define (square-of-sums n)
  (expt (reduce + 0 (iota (1+ n))) 2))

(define (difference n)
  (- (square-of-sums n) (sum-of-squares n)))
