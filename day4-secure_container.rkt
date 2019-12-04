#lang racket
(require rackunit)

(define (adjacent-same? str)
  (second
   ; reducee, passing along the last character and if we've already found adjacent characters
   (foldl (λ (c stat) (list c (or (second stat) (eq? (first stat) c))))
          '(#\U000000 #f)
          (string->list str))))

(module+ test
  (check-true (adjacent-same? "1223"))
  (check-true (adjacent-same? "1123"))
  (check-true (adjacent-same? "1233"))
  (check-false (adjacent-same? "123")))

(define (increasing? str)
   (second
    ; reduce, passing along the last character and if characters are increasing so far
    (foldl (λ (c stat) (list c (and (second stat) (char<=? (first stat) c))))
           '(#\U000000 #t)
           (string->list str))))

(module+ test
  (check-true (increasing? "123"))
  (check-false (increasing? "321"))
  (check-true (increasing? "1223")))

(define (valid? password)
  (and
   (= (string-length password) 6)
   (adjacent-same? password)
   (increasing? password)))

(module+ test
  (check-true (valid? "111111"))
  (check-false (valid? "223450"))
  (check-false (valid? "123789")))

(define (count-valid low high)
  (for/sum ([p (range low (+ high 1))]
            #:when (valid? (number->string p)))
    1))

(module+ main
  (displayln (count-valid 264793 803935)))