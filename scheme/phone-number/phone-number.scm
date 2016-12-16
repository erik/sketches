(define-module (phone-number)
  #:export (numbers area-code pprint)
  #:autoload (ice-9 format) (format))

(define (numbers number-string)
  (let ((stripped (string-filter char-numeric? number-string)))
    (cond
     ((or (< (string-length stripped) 10)
          (> (string-length stripped) 11)
          (and (= (string-length stripped) 11)
               (not (char=? (string-ref stripped 0) #\1))))
      "0000000000")
     ((= (string-length stripped) 11) (string-drop stripped 1))
     (else stripped))))

(define (area-code number-string)
  (string-take (numbers number-string) 3))

(define (pprint number-string)
  (let* ((num (numbers number-string))
         (a (string-take num 3))
         (b (string-take (string-drop num 3) 3))
         (c (string-take-right num 4)))
    (format #f "(~A) ~A-~A" a b c)))

(pprint "11234567890")
