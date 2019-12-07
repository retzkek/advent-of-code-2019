#lang racket

(require rackunit)

(define (read-orbit-map ip [om '()])
  (let* ([l (read-line ip)])
    ;(displayln l)
    (if (eof-object? l)
        om
        (read-orbit-map ip (cons (string-split l ")") om)))))

(define (count-orbits obj om)
  (let ([parent (filter (λ (x) (equal? (second x) obj)) om)])
    ;(displayln (list obj parent))
    (if (empty? parent)
       0
       (+ 1 (count-orbits (first (first parent)) om)))))

(define (sum-orbits om)
  (foldl + 0 (map (λ (x) (count-orbits (second x) om)) om)))

(module+ test
  (check-eq? (let ([om (read-orbit-map (open-input-string "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"))])
               (sum-orbits om))
             42))

(module+ main
  (sum-orbits
   (read-orbit-map
    (open-input-file
     (command-line
      #:program "orbit-map"
      #:args (filename)
      filename)))))
