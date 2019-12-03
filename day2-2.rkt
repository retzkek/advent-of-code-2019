#lang racket
(require "day2.rkt")

(define (find-args prog res)
  (for* ([noun (in-range 100)]
         [verb (in-range 100)]
         #:final (equal? (first (exe (append (list 1 noun verb) (drop prog 3)) 0)) res))
    (displayln (+ (* noun 100) verb))))
    
(module+ main
  (find-args (read-program (current-input-port)) 19690720)) 
