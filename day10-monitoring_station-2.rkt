#lang racket

(require rackunit
         math
         racket/struct)

(struct asteroid (x y) #:transparent)

; read map from string or port and return list of asteroid locations
(define (read-asteroid-map [m (current-input-port)])
  (let ([p (cond [(port? m) m]
                 [(string? m) (open-input-string m)])])
    (do ([asteroids '() (append asteroids
                                (map (λ (i) (asteroid i j))
                                     (indexes-of (string->list l) #\#)))]
         [j 0 (add1 j)]
         [l (read-line p) (read-line p)])
        ((eof-object? l) asteroids))))

(module+ test
  (define test-map (read-asteroid-map ".#..#
.....
#####
....#
...##"))
  (check-equal? test-map (list
                          (asteroid 1 0)
                          (asteroid 4 0)
                          (asteroid 0 2)
                          (asteroid 1 2)
                          (asteroid 2 2)
                          (asteroid 3 2)
                          (asteroid 4 2)
                          (asteroid 4 3)
                          (asteroid 3 4)
                          (asteroid 4 4))))

(define (can-see-between? a0 a1 asteroids)
  (let ([dx (- (asteroid-x a1) (asteroid-x a0))]
        [dy (- (asteroid-y a1) (asteroid-y a0))])
    (not (ormap (λ (a) (if (or (equal? a a0) (equal? a a1)) #f
                           (let ([dxa (- (asteroid-x a) (asteroid-x a0))]
                                 [dya (- (asteroid-y a) (asteroid-y a0))])
                             (cond
                               [(= dx dy 0) #f]
                               [(= dx 0) (and (= dxa 0) (< 0 (/ dya dy) 1))]
                               [(= dy 0) (and (= dya 0) (< 0 (/ dxa dx) 1))]
                               [else (and (< 0 (/ dxa dx) 1) (= (/ dxa dx) (/ dya dy)))]))))
                asteroids))))

(module+ test
  (check-true (can-see-between? (asteroid 1 0) (asteroid 2 1) test-map))
  (check-true (can-see-between? (asteroid 1 0) (asteroid 0 2) test-map))
  (check-false (can-see-between? (asteroid 1 0) (asteroid 4 3) test-map))
  (check-false (can-see-between? (asteroid 4 3) (asteroid 1 0) test-map)))

(define (count-can-see a0 asteroids)
  (count (λ (a) (if (equal? a a0)
                    #f
                    (can-see-between? a0 a asteroids)))
         asteroids))

(module+ test
  (check-eq? (count-can-see (asteroid 1 0) test-map) 7)
  (check-eq? (count-can-see (asteroid 4 0) test-map) 7)
  (check-eq? (count-can-see (asteroid 0 2) test-map) 6)
  (check-eq? (count-can-see (asteroid 1 2) test-map) 7)
  (check-eq? (count-can-see (asteroid 2 2) test-map) 7)
  (check-eq? (count-can-see (asteroid 2 2) test-map) 7)
  (check-eq? (count-can-see (asteroid 4 2) test-map) 5)
  (check-eq? (count-can-see (asteroid 4 3) test-map) 7)
  (check-eq? (count-can-see (asteroid 3 4) test-map) 8)
  (check-eq? (count-can-see (asteroid 4 4) test-map) 7))

(module+ test
  (check-eq? (count-can-see (asteroid 5 8) (read-asteroid-map "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")) 33)
  (check-eq? (count-can-see (asteroid 1 2) (read-asteroid-map "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")) 35)
  (check-eq? (count-can-see (asteroid 6 3) (read-asteroid-map ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")) 41)
  (check-eq? (count-can-see (asteroid 11 13) (read-asteroid-map ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")) 210))

(define (calculate-azimuth-from a0 a1)
  (let ([dx (- (asteroid-x a1) (asteroid-x a0))]
        [dy (- (asteroid-y a0) (asteroid-y a1))]) ; postive-y is down
    (cond
      [(and (= dx 0) (> dy 0)) 0]
      [(and (= dx 0) (< dy 0)) pi]
      [(and (> dx 0) (= dy 0)) (* pi 0.5)]
      [(and (< dx 0) (= dy 0)) (* pi 1.5)]
      [(and (> dx 0) (> dy 0)) (atan (/ dx dy))] ; quadrant I
      [(and (> dx 0) (< dy 0)) (- pi (atan (/ dx (- dy))))] ; quadrant II
      [(and (< dx 0) (< dy 0)) (+ pi (atan (/ (- dx) (- dy))))] ; quadrant III
      [(and (< dx 0) (> dy 0)) (- (* 2 pi) (atan (/ (- dx) dy)))] ; quadrant IV
      )))

(module+ test
  (define ɛ 0.00001)
  (check-= (calculate-azimuth-from (asteroid 4 4) (asteroid 4 3)) 0 ɛ)
  (check-= (calculate-azimuth-from (asteroid 4 4) (asteroid 5 3)) (* pi 0.25) ɛ)
  (check-= (calculate-azimuth-from (asteroid 4 4) (asteroid 5 4)) (* pi 0.5) ɛ)
  (check-= (calculate-azimuth-from (asteroid 4 4) (asteroid 5 5)) (* pi 0.75) ɛ)
  (check-= (calculate-azimuth-from (asteroid 4 4) (asteroid 4 5)) pi ɛ)
  (check-= (calculate-azimuth-from (asteroid 4 4) (asteroid 3 5)) (* pi 1.25) ɛ)
  (check-= (calculate-azimuth-from (asteroid 4 4) (asteroid 3 4)) (* pi 1.5) ɛ)
  (check-= (calculate-azimuth-from (asteroid 4 4) (asteroid 3 3)) (* pi 1.75) ɛ))

(struct target asteroid (azimuth) #:transparent)

(define (target-asteroids asteroids a0)
  (map (λ (a) (target (asteroid-x a) (asteroid-y a) (calculate-azimuth-from a0 a))) (filter (λ (x) (not (equal? x a0))) asteroids)))

(define (vaporize-round targets a0)
  (if (= (length targets) 0)
      '()
      (let-values ([(hit miss)
                    (partition (λ (x) (can-see-between? x a0 targets)) targets)])
        (append hit (vaporize-round miss a0)))))

(define (vaporize-asteroids asteroids a0)
  (let* ([targets (target-asteroids asteroids a0)]
         [target-order (sort targets (λ (a b) (< (target-azimuth a) (target-azimuth b))))])
    (vaporize-round target-order a0)))

(module+ test
  (let ([kia (vaporize-asteroids
         (read-asteroid-map ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##") (asteroid 11 13))])
    (check-equal? (take (struct->list (list-ref kia 0)) 2) '(11 12))
    (check-equal? (take (struct->list (list-ref kia 1)) 2) '(12 1))
    (check-equal? (take (struct->list (list-ref kia 2)) 2) '(12 2))
    (check-equal? (take (struct->list (list-ref kia 9)) 2) '(12 8))
    (check-equal? (take (struct->list (list-ref kia 19)) 2) '(16 0))
    (check-equal? (take (struct->list (list-ref kia 49)) 2) '(16 9))
    (check-equal? (take (struct->list (list-ref kia 99)) 2) '(10 16))
    (check-equal? (take (struct->list (list-ref kia 198)) 2) '(9 6))
    (check-equal? (take (struct->list (list-ref kia 199)) 2) '(8 2))
    (check-equal? (take (struct->list (list-ref kia 200)) 2) '(10 9))
    (check-equal? (take (struct->list (list-ref kia 298)) 2) '(11 1))))

(module+ main
  (let* ([m (read-asteroid-map
            (open-input-file
             (command-line
              #:args (filename)
              filename)))]
         [station (car (argmax (λ (x) (cdr x)) (map (λ (a) (cons a (count-can-see a m))) m)))]
         [kia (vaporize-asteroids m station)]
         [r (list-ref kia 199)])
    (displayln (+ (asteroid-y r) (* (asteroid-x r) 100)))))
