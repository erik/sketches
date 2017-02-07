(define-module (hamming)
  #:export (hamming-distance))

(define (hamming-distance str1 str2)
  (apply + (map (lambda (a b) (if (eq? a b) 0 1))
                (string->list str1)
                (string->list str2))))
