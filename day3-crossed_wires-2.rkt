#lang racket

(require rackunit)

(define (x point) (first point))
(define (y point) (second point))
(define (d point) (third point))

(define (next-point point step)
  (let ([dir (substring step 0 1)]
        [dist (string->number (substring step 1 (string-length step)))])
    (cond
      [(equal? dir "R") (list (+ (x point) dist) (y point) (+ (d point) dist))]
      [(equal? dir "L") (list (- (x point) dist) (y point) (+ (d point) dist))]
      [(equal? dir "U") (list (x point) (+ (y point) dist) (+ (d point) dist))]
      [(equal? dir "D") (list (x point) (- (y point) dist) (+ (d point) dist))]
      [else point])))

(module+ test
  (check-equal? (next-point '(0 0 0) "R10") '(10 0 10))
  (check-equal? (next-point '(0 0 0) "L10") '(-10 0 10))
  (check-equal? (next-point '(0 0 0) "U10") '(0 10 10))
  (check-equal? (next-point '(0 0 0) "D10") '(0 -10 10)))

(define (steps->lines steps [last '(0 0 0)] [lines '()])
  (let* ([next (next-point last (first steps))]
         [line (list last next)])
    (if (eq? (length steps) 1)
        (cons line lines)
        (steps->lines (rest steps) next (cons line lines)))))

(module+ test
  (check-equal? (steps->lines '("R8" "U5" "L5" "D3"))
                '(((3 5 18) (3 2 21)) ((8 5 13) (3 5 18)) ((8 0 8) (8 5 13)) ((0 0 0) (8 0 8)))))

(define (x0 line) (x (first line)))
(define (y0 line) (y (first line)))
(define (d0 line) (d (first line)))
(define (x1 line) (x (second line)))
(define (y1 line) (y (second line)))
(define (d1 line) (d (second line)))

(define (intersection l1 l2)
  (cond
    [(and (= (x0 l1) (x1 l1))     ; l1 vertical
          (= (y0 l2) (y1 l2))     ; l2 horizontal
          (or (< (y0 l1) (y0 l2) (y1 l1))
              (> (y0 l1) (y0 l2) (y1 l1)))  ; l1 span l2?
          (or (< (x0 l2) (x0 l1) (x1 l2))
              (> (x0 l2) (x0 l1) (x1 l2)))) ; l2 span l1?
     (list (x0 l1) (y0 l2)
           ; add total delay from both lines
           (+ (d0 l1)
              (if (< (y0 l1) (y0 l2)) (- (y0 l2) (y0 l1)) (- (y0 l1) (y0 l2)))
              (d0 l2)
              (if (< (x0 l1) (x0 l2)) (- (x0 l2) (x0 l1)) (- (x0 l1) (x0 l2)))))]
    [(and (= (y0 l1) (y1 l1))   ; l1 horizontal
          (= (x0 l2) (x1 l2))   ; l2 vertical
          (or (< (x0 l1) (x0 l2) (x1 l1))
              (> (x0 l1) (x0 l2) (x1 l1)))  ; l1 span l2?
          (or (< (y0 l2) (y0 l1) (y1 l2))
              (> (y0 l2) (y0 l1) (y1 l2)))) ; l2 span l1?
     (list (x0 l2) (y0 l1)
           ; add total delay from both lines
           (+ (d0 l1)
              (if (< (x0 l1) (x0 l2)) (- (x0 l2) (x0 l1)) (- (x0 l1) (x0 l2)))
              (d0 l2)
              (if (< (y0 l1) (y0 l2)) (- (y0 l2) (y0 l1)) (- (y0 l1) (y0 l2)))))]
    [else '()]))

(module+ test
  (check-equal? (intersection '((0 0 0) (10 0 10)) '((5 -1 0) (5 1 2))) '(5 0 6))
  (check-equal? (intersection '((0 0 0) (0 10 10)) '((-3 5 0) (11 5 14))) '(0 5 8))
  (check-equal? (intersection '((0 0 0) (10 0 0)) '((5 2 0) (5 4 2))) '()))

(define (manhattan-distance p0 [p1 '(0 0 0)])
    (+ (if (> (x p1) (x p0)) (- (x p1) (x p0)) (- (x p0) (x p1)))
       (if (> (y p1) (y p0)) (- (y p1) (y p0)) (- (y p0) (y p1)))))

(module+ test
  (check-eq? (manhattan-distance '(3 3 21)) 6)
  (check-eq? (manhattan-distance '(-3 -3 17)) 6)
  (check-eq? (manhattan-distance '(5 2 91)) 7)
  (check-eq? (manhattan-distance '(2 5 88) '(-5 -2 33)) 14))

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

(define (min-delay [ip (current-input-port)])
  (let* ([wire1 (steps->lines (string-split (read-line ip) ","))]
         [wire2 (steps->lines (string-split (read-line ip) ","))]
         [intersections (filter-not empty?
                                    (for*/list ([l1 wire1]
                                                [l2 wire2])
                                      (intersection l1 l2)))])
    ;;(displayln intersections)
    (first (sort (map third intersections) <))))

(module+ test
   (check-eq? (min-delay (open-input-string "R8,U5,L5,D3
U7,R6,D4,L4")) 30)
  (check-eq? (min-delay (open-input-string "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")) 610)
  (check-eq? (min-delay (open-input-string "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) 410))

(module+ main (min-delay))