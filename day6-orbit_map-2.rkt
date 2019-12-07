#lang racket

(require rackunit)



(define (read-orbit-map ip [om '()])
  (let* ([l (read-line ip)])
    ;(displayln l)
    (if (eof-object? l)
        om
        (read-orbit-map ip (cons (string-split l ")") om)))))

(define *test-orbit-map*
  (read-orbit-map (open-input-string "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")))

(module+ test
  (check-equal? *test-orbit-map*
                '(("I" "SAN")
                  ("K" "YOU")
                  ("K" "L")
                  ("J" "K")
                  ("E" "J")
                  ("D" "I")
                  ("G" "H")
                  ("B" "G")
                  ("E" "F")
                  ("D" "E")
                  ("C" "D")
                  ("B" "C")
                  ("COM" "B"))))

(define (list-orbits obj om)
  (let ([parent (filter (λ (x) (equal? (second x) obj)) om)])
    ;(displayln (list obj parent))
    (if (empty? parent)
        '()
        (cons (first (first parent))
              (list-orbits (first (first parent)) om)))))

(module+ test
  (check-equal? (list-orbits "J" *test-orbit-map*)
                '("E" "D" "C" "B" "COM")))

(define (count-orbits obj om)
  (length (list-orbits obj om)))

(module+ test
  (check-equal? (count-orbits "J" *test-orbit-map*) 5))

(define (sum-orbits om)
  (foldl + 0 (map (λ (x) (count-orbits (second x) om)) om)))

(module+ test
  (check-eq? (sum-orbits *test-orbit-map*) 54))

(define (transfers from to om)
  (let ([from-orbits (list-orbits from om)]
        [to-orbits (list-orbits to om)])
    (apply
     min
     (filter-map
      (λ (x)
        (let ([a (index-of from-orbits x)]
              [b (index-of to-orbits x)])
          (if b (+ a b) #f)))
      from-orbits))))

(module+ test
  (check-eq? (transfers "YOU" "SAN" *test-orbit-map*) 4))

(module+ main
  (let ([om (read-orbit-map
    (open-input-file
     (command-line
      #:program "orbit-map"
      #:args (filename)
      filename)))])
    (displayln (transfers "YOU" "SAN" om))))
