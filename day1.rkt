#lang racket

(require rackunit)

(define (fuel mass [include-fuel? #f])
  (let ([f0 (- (floor (/ mass 3)) 2)])
    ;;(writeln f0)
    (cond
      [(< mass 6) 0]
      [(not include-fuel?) f0]
      [(+ f0 (fuel f0 #t))])))

(module+ test
  (check-eq? (fuel 12) 2)
  (check-eq? (fuel 14) 2)
  (check-eq? (fuel 1969) 654)
  (check-eq? (fuel 100756) 33583)
  (check-eq? (fuel 14 #t) 2)
  (check-eq? (fuel 1969 #t) 966)
  (check-eq? (fuel 100756 #t) 50346))

(module+ main
  (for/sum ([m (in-port)]) (fuel m #t)))