#lang racket

(require "intcode.rkt")

(define (test-location x y)
  (define ch (make-channel))
  (define program (open-input-file "day19.input.txt"))
  (define intcode (new intcode% [output ch] [program program]))
  (close-input-port program)
  (define brain (thread (λ () (send intcode exe 0))))
  (for ([inp (list x y)])
    (let ([r (sync/timeout 10 ch (thread-dead-evt brain))])
      (when (not (eq? -99 r))
        (raise (error (format "expected prompt, got ~a" r)))))
    (channel-put ch inp))
  (channel-get ch))

(for*/sum ([y (in-range 50)]
           [x (in-range 50)])
  (let ([r (test-location x y)])
    (display r)
    (when (= x 49) (newline))
    r))
