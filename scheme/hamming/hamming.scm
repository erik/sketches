(define-module (hamming)
  #:export (hamming-distance))

;; (zip '(1 2 3) '(4 5 6)) => ((1 4) (2 5) (3 6))
(define (zip a b) (map list a b))

(define (hamming-distance str1 str2)
  (apply + (map (lambda (a) (if (eq? (car a) (cadr a)) 0 1))
                (zip (string->list str1) (string->list str2)))))
