#lang racket

(require rackunit)

(struct reactant (id amt) #:transparent)

(define reactant-regexp #px"^\\s*(\\d+)\\s+(\\w+)\\s*$")
(define reaction-regexp #px"^((?:\\s*\\d+\\s+\\w+,?)+)\\s*=>\\s+(\\d+\\s+\\w+)$")

(define (parse-reactant rt)
  (match (regexp-match reactant-regexp rt)
    [(list _ amt id) (reactant id (string->number amt))]
    [_ (raise (format "bad reactant: ~a" rt))]))

(module+ test
  (check-equal? (parse-reactant "10 ORE")
                (reactant "ORE" 10)))

(define (parse-reaction rx)
  (match (regexp-match reaction-regexp rx)
    [(list _ reactants result)
     (cons (parse-reactant result) (map parse-reactant (string-split reactants ",")))]
    [_ (raise (format "bad reaction: ~a" rx))]))

(module+ test
  (check-equal? (parse-reaction "7 A, 1 E => 1 FUEL")
                (list (reactant "FUEL" 1) (reactant "A" 7) (reactant "E" 1))))

#;
(define (read-reactions in)
  (define ip (cond [(port? in) in]
                   [(string? in) (open-input-string in)]))
  (define (read-reaction ip [reactions '()])
    (define l (read-line ip))
    (if (eof-object? l)
        reactions
        (let ([new-moon (match (or (regexp-match vector-regexp l)
                                   (regexp-match #px"^pos=(.*),\\s+vel=(.*)$" l))
                          [(list _ x y z) (moon (read-vector l) #(0 0 0))]
                          [(list _ pos vel) (moon (read-vector pos) (read-vector vel))]
                          [_ (raise (format "that's no moon: ~a" in))])])
          (read-moon ip (cons new-moon moons)))))
  (read-moon ip))


#;
(module+ test
  (define test-reactions "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"))
