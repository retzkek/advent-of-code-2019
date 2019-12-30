#lang racket
(require rackunit)

;; these functions return the new position for the card at position i in a deck of length l
(define (deal-new l i) (- l i 1))
(module+ test
  (check-equal? (shuffle-deck (range 10) (curry deal-new 10)) '(9 8 7 6 5 4 3 2 1 0)))

(define (cut n l i) (modulo (- i (if (> n 0) n (+ l n))) l))
(module+ test
  (check-equal? (shuffle-deck (range 10) (curry cut 3 10)) '(3 4 5 6 7 8 9 0 1 2))
  (check-equal? (shuffle-deck (range 10) (curry cut -4 10)) '(6 7 8 9 0 1 2 3 4 5)))

(define (deal-n n l i) (modulo (* i n) l))
(module+ test
  (check-equal? (shuffle-deck (range 10) (curry deal-n 3 10)) '(0 7 4 1 8 5 2 9 6 3)))

;; shuffle deck according to position update function f
(define (shuffle-deck deck f)
  (update-deck deck (map f (range (length deck)))))

;; move cards from deck to new positions
(define (update-deck deck new-pos)
  (map (lambda (x) (list-ref deck (index-of new-pos x))) (range (length deck))))

(define (parse-shuffle s)
  (match (or (regexp-match #px"^deal into (new) stack$" s)
             (regexp-match #px"^(cut) (-?\\d+)$" s)
             (regexp-match #px"^(deal) with increment (\\d+)$" s))
    [(list _ "new") (curry deal-new)]
    [(list _ "cut" n) (curry cut (string->number n))]
    [(list _ "deal" n) (curry deal-n (string->number n))]))

(define (read-shuffles in)
  (define ip (cond [(port? in) in]
                   [(string? in) (open-input-string in)]))
  (define (read-next-shuffle ip [shuffles '()])
    (define s (read-line ip))
    (if (eof-object? s)
        shuffles
        (read-next-shuffle ip (cons (parse-shuffle s) shuffles))))
  (reverse (read-next-shuffle ip)))

(define (shuffle-position steps l i)
  (foldl (lambda (x i) (x l i)) i steps))

(define (shuffle deck steps)
  (update-deck deck (map (curry shuffle-position steps (length deck)) (range (length deck)))))

;(define (shuffle deck steps)
;  (define l (length deck))
;  (foldl (lambda (x d) (shuffle-deck d x)) (range l) (map (lambda (x) (x l)) steps)))

(module+ test
  (check-equal?
   '(0 3 6 9 2 5 8 1 4 7)
   (shuffle
    (range 10)
    (read-shuffles "deal with increment 7
deal into new stack
deal into new stack")))
  (check-equal?
   '(3 0 7 4 1 8 5 2 9 6)
   (shuffle
    (range 10)
    (read-shuffles "cut 6
deal with increment 7
deal into new stack")))
  (check-equal?
   '(6 3 0 7 4 1 8 5 2 9)
   (shuffle
    (range 10)
    (read-shuffles "deal with increment 7
deal with increment 9
cut -2")))
  (check-equal?
   '(9 2 5 8 1 4 7 0 3 6)
   (shuffle
    (range 10)
    (read-shuffles "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1"))))

(module+ main
  (define steps (read-shuffles (open-input-file "day22.input.txt")))
  (displayln (shuffle-position steps 10007 2019)))