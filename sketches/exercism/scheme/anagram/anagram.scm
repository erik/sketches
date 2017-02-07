(define-module (anagram)
  #:export (anagrams-for))


(define (sort-word word)
  (sort (string->list (string-downcase word)) char<?))

(define (anagrams-for word choices)
  (let ((sorted (sort-word word)))
    (filter (lambda (w) (and (equal? (sort-word w) sorted)
                             (not (equal? w word))))
            choices)))
