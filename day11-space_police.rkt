#lang racket

(require rackunit
         "intcode.rkt")

(struct panel (x y) #:transparent)

(define (panel-color hull x y)
  (if (hash-has-key? hull (panel x y))
      (hash-ref hull (panel x y))
      0))

(define (ehpr program)
  (let* ([ch (make-channel)]
         [brain (thread (λ () (exe (read-program program) 0 ch)))]
         [hull (make-hash)]
         [turns (hash 'up '(left right) 'right '(up down) 'down '(right left) 'left '(down up))])
    (define (run turn dir x y)
      (let ([new-dir (list-ref (hash-ref turns dir) turn)])
        ;(displayln (list x y (panel-color hull x y)))
        (thread-send brain (panel-color hull x y))
        (if (thread-dead? brain)
            hull
            (begin
              (hash-set! hull (panel x y) (channel-get ch))
              (run (channel-get ch)
                   new-dir
                   (cond [(eq? new-dir 'left) (- x 1)]
                         [(eq? new-dir 'right) (+ x 1)]
                         [else x])
                   (cond [(eq? new-dir 'up) (- y 1)]
                         [(eq? new-dir 'down) (+ y 1)]
                         [else y]))))))
    (run 0 'up 0 0)))

(module+ main
  (ehpr (open-input-file "day11.input.txt")))
