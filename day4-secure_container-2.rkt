#lang racket
(require rackunit)

; turn password into "compact" form describing (character, # of repetitions).
; e.g. "1123" -> '((#\1 2) (#\2 1) (#\3 1))
(define (pack password)
  (foldl (lambda (c res) (if (empty? res)
                             (list (list c 1))
                             (if (eq? (caar res) c)
                                 (cons (list c (+ (cadar res) 1)) (rest res))
                                 (cons (list c 1) res))))
         '()
         (string->list password)))

(define (adjacent-same? pw [exactly #f])
  (let ([t (if exactly
               (λ (x) (= x exactly))
               (λ (x) (> x 1)))])
    (ormap (λ (c) (t (second c))) pw)))

(module+ test
  (check-true (adjacent-same? (pack "1223")))
  (check-true (adjacent-same? (pack "1123")))
  (check-true (adjacent-same? (pack "1233")))
  (check-false (adjacent-same? (pack "123"))))

(define (increasing? pw)
   (second
    ; reduce, passing along the last character and if characters are increasing so far
    (foldl (λ (c stat) (list c (and (second stat) (char>=? (first stat) c))))
           '(#\U10FFFF #t)
           (map (λ (c) (first c)) pw))))

(module+ test
  (check-true (increasing? (pack "123")))
  (check-false (increasing? (pack "321")))
  (check-true (increasing? (pack "1223"))))

(define (valid? password)
  (let ([pw (pack password)])
    (and
     (= (string-length password) 6)
     (adjacent-same? pw 2)
     (increasing? pw))))

(module+ test
  (check-true (valid? "112233"))
  (check-false (valid? "123444"))
  (check-true (valid? "111122")))

(define (count-valid low high)
  (for/sum ([p (range low (+ high 1))]
            #:when (valid? (number->string p)))
    1))

(module+ main
  (displayln (count-valid 264793 803935)))
