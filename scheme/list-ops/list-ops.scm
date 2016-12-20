(define-module (list-ops)
  #:export (my-length
            my-reverse
            my-map
            my-filter
            my-fold
            my-append
            my-concatenate))

(define (my-fold-left fn init lst)
  (if (null? lst)
      init
      (my-fold-left fn (fn (car lst) init) (cdr lst))))

(define (my-fold-right fn init lst)
  (if (null? lst)
      init
      (my-fold-left fn init (my-reverse lst))))

(define (my-length lst)
  (my-fold-left (lambda (_ acc) (1+ acc)) 0 lst))

(define (my-reverse lst)
  (my-fold-left (lambda (x acc) (cons x acc)) '() lst))

(define (my-map fn lst)
  (my-fold-right (lambda (x acc) (cons (fn x) acc)) '() lst))

(define (my-filter fn lst)
  (my-fold-right (lambda (x acc) (if (fn x) (cons x acc) acc)) '() lst))

;; not specified, default to a left fold
(define my-fold my-fold-left)

(define (my-append l1 l2)
  (my-fold-right cons l2 l1))

(define (my-concatenate lst)
  (my-fold-right my-append '() lst))
