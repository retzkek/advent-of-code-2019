#lang racket

(require "intcode.rkt")

(define ch (make-channel))
(define program (open-input-file "day17.input.txt"))
(define intcode (new intcode% [output ch] [program program]))
(send intcode poke! 0 2) ; turn on vacuum bot
(define brain (thread (λ () (send intcode exe 0))))

(define (display-until ch p)
  (define r (channel-get ch))
  (if (p r)
      r
      (begin
        (display (integer->char r))
        (display-until ch p))))

(define (send-ascii-line ch s)
  (for ([c (in-list (map char->integer (string->list s)))])
    (display-until ch (λ (x) (= x -99)))
    (channel-put ch c)))

(send-ascii-line ch "A,B,A,B,C,B,C,A,B,C\n")
(send-ascii-line ch "R,4,R,10,R,8,R,4\n")
(send-ascii-line ch "R,10,R,6,R,4\n")
(send-ascii-line ch "R,4,L,12,R,6,L,12\n")
(send-ascii-line ch "n\n")                     ; don't display video
;(display (channel-get ch)) ; get dust collected
(display-until ch (λ (x) (> x 128)))
