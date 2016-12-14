(define-module (hello-world)
  #:export (hello))

(define* (hello #:optional (name "World"))
  (string-append "Hello, " name "!"))
