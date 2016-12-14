(define-module (leap-year)
  #:export (leap-year?))

(define (leap-year? year)
  (or (zero? (modulo year 400))
      (and (not (zero? (modulo year 100)))
           (zero? (modulo year 4)))))
