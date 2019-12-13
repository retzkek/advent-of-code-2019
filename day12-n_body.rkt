#lang racket

(require rackunit)

(struct v3 (x y z) #:transparent)

(define vector-regexp #px"^\\s*<x=\\s*([\\d\\.-]+),[\\s]*y=\\s*([\\d\\.-]+),[\\s]*z=\\s*([\\d\\.-]+)>\\s*$")

(define (read-vector in)
  (match (regexp-match vector-regexp in)
    [(list _ x y z) (vector (string->number x) (string->number y) (string->number z))]
    [_  (raise (format "not a vector: ~a" in))]))

(struct moon (position velocity) #:transparent)

(define (read-moons in)
  (define ip (cond [(port? in) in]
                   [(string? in) (open-input-string in)]))
  (define (read-moon ip [moons '()])
    (define l (read-line ip))
    (if (eof-object? l)
        moons
        (let ([new-moon (match (or (regexp-match vector-regexp l)
                                   (regexp-match #px"^pos=(.*),\\s+vel=(.*)$" l))
                          [(list _ x y z) (moon (read-vector l) #(0 0 0))]
                          [(list _ pos vel) (moon (read-vector pos) (read-vector vel))]
                          [_ (raise (format "that's no moon: ~a" in))])])
          (read-moon ip (cons new-moon moons)))))
  (read-moon ip))

(define (step moons)
  (define (gravity moons a-moon)
    ; calculate the change in velocity to a-moon due to gravity from the other moons
    (foldl (λ (m delta)
             (vector-map
              + delta
              (vector-map
               (λ (a b) (cond [(< a b) -1]
                              [(> a b) 1]
                              [else 0]))
               (moon-position m)
               (moon-position a-moon))))
           #(0 0 0)
           moons))
  (define (update-velocity moons a-moon)
    (struct-copy moon a-moon [velocity
                              (vector-map + (moon-velocity a-moon)
                                          (gravity moons a-moon))]))
  (define (update-position a-moon)
    (struct-copy moon a-moon [position
                              (vector-map + (moon-position a-moon)
                                          (moon-velocity a-moon))]))
  (map (λ (m) (update-position (update-velocity moons m))) moons))

(module+ test
  (define test-moons (read-moons "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>"))
  (check-equal? test-moons
                (list
                 (moon #(3 5 -1) #(0 0 0))
                 (moon #(4 -8 8) #(0 0 0))
                 (moon #(2 -10 -7) #(0 0 0))
                 (moon #(-1 0 2) #(0 0 0))))
  (define test-steps
    (list
     (read-moons "pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>
pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>
pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>
pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>")
     (read-moons "pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>")
     (read-moons "pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>")
     (read-moons "pos=<x= 5, y=-6, z=-1>, vel=<x= 0, y=-3, z= 0>
pos=<x= 0, y= 0, z= 6>, vel=<x=-1, y= 2, z= 4>
pos=<x= 2, y= 1, z=-5>, vel=<x= 1, y= 5, z=-4>
pos=<x= 1, y=-8, z= 2>, vel=<x= 0, y=-4, z= 0>")
     (read-moons "pos=<x= 2, y=-8, z= 0>, vel=<x=-3, y=-2, z= 1>
pos=<x= 2, y= 1, z= 7>, vel=<x= 2, y= 1, z= 1>
pos=<x= 2, y= 3, z=-6>, vel=<x= 0, y= 2, z=-1>
pos=<x= 2, y=-9, z= 1>, vel=<x= 1, y=-1, z=-1>")
     (read-moons "pos=<x=-1, y=-9, z= 2>, vel=<x=-3, y=-1, z= 2>
pos=<x= 4, y= 1, z= 5>, vel=<x= 2, y= 0, z=-2>
pos=<x= 2, y= 2, z=-4>, vel=<x= 0, y=-1, z= 2>
pos=<x= 3, y=-7, z=-1>, vel=<x= 1, y= 2, z=-2>")
     (read-moons "pos=<x=-1, y=-7, z= 3>, vel=<x= 0, y= 2, z= 1>
pos=<x= 3, y= 0, z= 0>, vel=<x=-1, y=-1, z=-5>
pos=<x= 3, y=-2, z= 1>, vel=<x= 1, y=-4, z= 5>
pos=<x= 3, y=-4, z=-2>, vel=<x= 0, y= 3, z=-1>")
     (read-moons "pos=<x= 2, y=-2, z= 1>, vel=<x= 3, y= 5, z=-2>
pos=<x= 1, y=-4, z=-4>, vel=<x=-2, y=-4, z=-4>
pos=<x= 3, y=-7, z= 5>, vel=<x= 0, y=-5, z= 4>
pos=<x= 2, y= 0, z= 0>, vel=<x=-1, y= 4, z= 2>")
     (read-moons "pos=<x= 5, y= 2, z=-2>, vel=<x= 3, y= 4, z=-3>
pos=<x= 2, y=-7, z=-5>, vel=<x= 1, y=-3, z=-1>
pos=<x= 0, y=-9, z= 6>, vel=<x=-3, y=-2, z= 1>
pos=<x= 1, y= 1, z= 3>, vel=<x=-1, y= 1, z= 3>")
     (read-moons "pos=<x= 5, y= 3, z=-4>, vel=<x= 0, y= 1, z=-2>
pos=<x= 2, y=-9, z=-3>, vel=<x= 0, y=-2, z= 2>
pos=<x= 0, y=-8, z= 4>, vel=<x= 0, y= 1, z=-2>
pos=<x= 1, y= 1, z= 5>, vel=<x= 0, y= 0, z= 2>")
     (read-moons "pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>")))
  (define (check-steps moons steps)
    (unless (empty? steps)
      (check-equal? moons (first steps))
      (check-steps (step moons) (rest steps))))
  (check-steps test-moons test-steps))
