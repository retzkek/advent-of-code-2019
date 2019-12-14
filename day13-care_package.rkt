#lang racket

(require "intcode.rkt")

(define (game program)
  (let* ([ch (make-channel)]
         [brain (thread (λ () (exe (read-program program) 0 ch)))]
         [board (make-hash)])
    (define (run board)
      (define x (sync ch (thread-dead-evt brain)))
      (define y (sync ch (thread-dead-evt brain)))
      (define tile-num (sync ch (thread-dead-evt brain)))
      (if (thread-dead? brain)
          (count (λ (x) (eq? x 2)) (hash-values board))
          (run (hash-set board (vector x y) tile-num))))
    (run (make-immutable-hash))))

(module+ main
  (displayln (game (open-input-file "day13.input.txt"))))
