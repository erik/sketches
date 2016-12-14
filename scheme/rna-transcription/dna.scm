(define-module (dna)
  #:export (to-rna))

(define (to-rna dna)
  (string-map
   (lambda (c)
     (cond
      ((char=? c #\G) #\C)
      ((char=? c #\C) #\G)
      ((char=? c #\T) #\A)
      ((char=? c #\A) #\U)))
   dna))
