#lang racket

(require rackunit)

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

(module+ main
  (let ([m (read-asteroid-map
            (open-input-file
             (command-line
              #:args (filename)
              filename)))])
    (displayln (argmax (λ (x) (cdr x)) (map (λ (a) (cons a (count-can-see a m))) m)))))
