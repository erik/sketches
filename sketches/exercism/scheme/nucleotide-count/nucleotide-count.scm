(define-module (nucleotide-count)
  #:export (nucleotide-counts dna-count))

(define nucleotides '(#\A #\C #\G #\T))

(define (dna-count nct strand)
  (if (not (member nct nucleotides))
      (throw "bad nucleotide given")
      (string-length (string-filter (lambda (c) (char=? nct c)) strand))))

(define (nucleotide-counts strand)
  (map (lambda (nct) (cons nct (dna-count nct strand))) nucleotides))
