#lang racket

(require rackunit)

(define (next-point point step)
  (let ([dir (substring step 0 1)]
        [dist (string->number (substring step 1 (string-length step)))])
    (cond
      [(equal? dir "R") (list (+ (first point) dist) (second point))]
      [(equal? dir "L") (list (- (first point) dist) (second point))]
      [(equal? dir "U") (list (first point) (+ (second point) dist))]
      [(equal? dir "D") (list (first point) (- (second point) dist))]
      [else point])))

(module+ test
  (check-equal? (next-point '(0 0) "R10") '(10 0))
  (check-equal? (next-point '(0 0) "L10") '(-10 0))
  (check-equal? (next-point '(0 0) "U10") '(0 10))
  (check-equal? (next-point '(0 0) "D10") '(0 -10)))

(define (steps->lines steps [last '(0 0)] [lines '()])
  (let* ([next (next-point last (first steps))]
         [line (append last next)])
    (if (eq? (length steps) 1)
        (cons line lines)
        (steps->lines (rest steps) next (cons line lines)))))

(module+ test
  (check-equal? (steps->lines '("R8" "U5" "L5" "D3")) '((3 5 3 2) (8 5 3 5) (8 0 8 5) (0 0 8 0))))

(define (intersection l1 l2)
  (cond
    [(and (= (first l1) (third l1))     ; l1 vertical
          (= (second l2) (fourth l2))   ; l2 horizontal
          (or (< (second l1) (second l2) (fourth l1))
              (> (second l1) (second l2) (fourth l1)))
          (or (< (first l2) (first l1) (third l2))
              (> (first l2) (first l1) (third l2))))
     (list (first l1) (second l2))]
    [(and (= (second l1) (fourth l1))   ; l1 horizontal
          (= (first l2) (third l2))     ; l2 vertical
          (or (< (first l1) (first l2) (third l1))
              (> (first l1) (first l2) (third l1)))
          (or (< (second l2) (second l1) (fourth l2))
              (> (second l2) (second l1) (fourth l2))))
     (list (first l2) (second l1))]
    [else '()]))

(module+ test
  (check-equal? (intersection '(0 0 10 0) '(5 -1 5 1)) '(5 0))
  (check-equal? (intersection '(0 0 0 10) '(-3 5 11 5)) '(0 5))
  (check-equal? (intersection '(0 0 10 0) '(5 2 5 4)) '()))

(define (manhattan-distance p0 [p1 '(0 0)])
  (let ([x0 (first p0)]
        [y0 (second p0)]
        [x1 (first p1)]
        [y1 (second p1)])
    (+ (if (> x1 x0) (- x1 x0) (- x0 x1))
       (if (> y1 y0) (- y1 y0) (- y0 y1)))))

(module+ test
  (check-eq? (manhattan-distance '(3 3)) 6)
  (check-eq? (manhattan-distance '(-3 -3)) 6)
  (check-eq? (manhattan-distance '(5 2)) 7)
  (check-eq? (manhattan-distance '(2 5) '(-5 -2)) 14))

(define (min-cross [ip (current-input-port)])
  (let* ([wire1 (steps->lines (string-split (read-line ip) ","))]
         [wire2 (steps->lines (string-split (read-line ip) ","))]
         [intersections (filter-not empty?
                                    (for*/list ([l1 wire1]
                                                [l2 wire2])
                                      (intersection l1 l2)))])
    ;;(displayln intersections)
    (first (sort (map manhattan-distance intersections) <))))

(module+ test
   (check-eq? (min-cross (open-input-string "R8,U5,L5,D3
U7,R6,D4,L4")) 6)
  (check-eq? (min-cross (open-input-string "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")) 159)
  (check-eq? (min-cross (open-input-string "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) 135))

(module+ main (min-cross))