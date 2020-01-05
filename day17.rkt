#lang racket

(require "intcode.rkt")

(define ascii%
  (class intcode%
    (define ch (make-channel))
    (super-new [output ch])
    (define brain (thread (λ () (send this exe 0))))
    (define scaffold (make-hash))
    (define/public (run [x 0] [y 0] [isects '()])
      (define r (sync ch (thread-dead-evt brain)))
      (if (thread-dead? brain)
          (foldr + 0 (map (λ (x) (* (car x) (cdr x))) isects))
          (begin
            (display (integer->char r))
            (when (= r 35)
              (hash-set! scaffold (cons x y) r))
            (run (if (= r 10) 0 (add1 x))
                 (if (= r 10) (add1 y) y)
                 (if (and (hash-has-key? scaffold (cons x y))
                          (hash-has-key? scaffold (cons (- x 1) (- y 1)))
                          (hash-has-key? scaffold (cons x (- y 1)))
                          (hash-has-key? scaffold (cons (+ x 1) (- y 1)))
                          (hash-has-key? scaffold (cons x (- y 2))))
                     (cons (cons x (- y 1)) isects)
                     isects)))))))

(module+ main
  (send (new ascii% [program (open-input-file "day17.input.txt")])
        run))
