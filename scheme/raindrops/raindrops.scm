(define-module (raindrops)
  #:export (convert))


(define (convert num)
  (let ((str (string-append
              (if (zero? (modulo num 3)) "Pling" "")
              (if (zero? (modulo num 5)) "Plang" "")
              (if (zero? (modulo num 7)) "Plong" ""))))
    (if (string-null? str)
        (number->string num)
        str)))
