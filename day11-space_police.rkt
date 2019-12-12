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
    (define (run dir x y)
      ;(displayln (list x y (panel-color hull x y)))
      (thread-send brain (panel-color hull x y))
      (let ([new-color (sync ch (thread-dead-evt brain))]
            [turn (sync ch (thread-dead-evt brain))])
        (if (thread-dead? brain)
            hull
            (let ([new-dir (list-ref (hash-ref turns dir) turn)])
              (hash-set! hull (panel x y) new-color)
              (run new-dir
                   (cond [(eq? new-dir 'left) (- x 1)]
                         [(eq? new-dir 'right) (+ x 1)]
                         [else x])
                   (cond [(eq? new-dir 'up) (- y 1)]
                         [(eq? new-dir 'down) (+ y 1)]
                         [else y]))))))
    (run 'up 0 0)))

(module+ main
  (hash-count (ehpr (open-input-file "day11.input.txt"))))
