(define-module (bob)
  #:export (response-for))

(use-modules (ice-9 regex))

(define (response-for str)
  (cond
   ((string-match "[A-Z]+[^a-z]*(!|\\?)*[^\\.]$" str) "Whoa, chill out!")
   ((string-match "\\?$" str) "Sure.")
   ((zero? (string-length (string-trim str))) "Fine. Be that way!")
   (else "Whatever.")))
