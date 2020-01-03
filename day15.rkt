#lang racket

(require "intcode.rkt")

(define repair-droid%
  (class intcode%
    (define ch (make-channel))
    (super-new [output ch])
    (define brain (thread (λ () (send this exe 0))))
    (define dirs (make-hash '((N . 1) (S . 2) (W . 3) (E . 4))))
    (define cur-dir 'N)
    (define cur-x 0)
    (define min-x 0)
    (define max-x 0)
    (define cur-y 0)
    (define min-y 0)
    (define max-y 0)
    (define cur-z 1)
    (define map (make-hash))
    (define/private (draw-map)
      (for* ([y (in-range max-y (sub1 min-y) -1)]
             [x (in-range min-x (add1 max-x))])
        (display
         (cond
           [(and (eq? x cur-x) (eq? y cur-y)) "*"]
           [(hash-has-key? map (cons x y))
            (case (hash-ref map (cons x y))
              ((0) "#")
              ((1) ".")
              ((2) "@"))]
           [else " "]))
        (when (eq? x max-x) (newline))))

    (define/private (n x y) (cons x (add1 y)))
    (define/private (s x y) (cons x (sub1 y)))
    (define/private (e x y) (cons (add1 x) y))
    (define/private (w x y) (cons (sub1 x) y))
    (define/private (move dir)
      (when (not (eq? -99 (sync/timeout 10 ch)))
        (raise (error "expected prompt")))
      (channel-put ch (hash-ref dirs dir))
      (define r (channel-get ch))
      (case dir
        [(N) (hash-set! map (n cur-x cur-y) r)
             (when (> (add1 cur-y) max-y) (set! max-y (add1 cur-y)))]
        [(S) (hash-set! map (s cur-x cur-y) r)
             (when (< (sub1 cur-y) min-y) (set! min-y (sub1 cur-y)))]
        [(W) (hash-set! map (w cur-x cur-y) r)
             (when (< (sub1 cur-x) min-x) (set! min-x (sub1 cur-x)))]
        [(E) (hash-set! map (e cur-x cur-y) r)
             (when (> (add1 cur-x) max-x) (set! max-x (add1 cur-x)))])
      (if (eq? r 0)
          r
          (begin
            (set! cur-z r)
            (set! cur-dir dir)
            (case dir
              [(N) (set! cur-y (add1 cur-y))]
              [(S) (set! cur-y (sub1 cur-y))]
              [(W) (set! cur-x (sub1 cur-x))]
              [(E) (set! cur-x (add1 cur-x))])
            r)))

    ;; mapping-based search
    (define/public (map-search)
      (define (go)
        (hash-set! map (cons cur-x cur-y) cur-z)
        (displayln (format "~a ~a ~a ~a" cur-dir cur-x cur-y cur-z))
        (draw-map)
        (sleep 0.1)
        (cond
          [(eq? 2 (hash-ref map (cons cur-x cur-y)))
           (cons cur-x cur-y)]
          ;; explore forward
          [(and (eq? cur-dir 'N)
                (not (hash-has-key? map (n cur-x cur-y)))
                (< 0 (move 'N)))
           (go)]
          [(and (eq? cur-dir 'S)
                (not (hash-has-key? map (s cur-x cur-y)))
                (< 0 (move 'S)))
           (go)]
          [(and (eq? cur-dir 'W)
                (not (hash-has-key? map (w cur-x cur-y)))
                (< 0 (move 'W)))
           (go)]
          [(and (eq? cur-dir 'E)
                (not (hash-has-key? map (e cur-x cur-y)))
                (< 0 (move 'E)))
           (go)]
          ;; explore right
          [(and (eq? cur-dir 'N)
                (not (hash-has-key? map (e cur-x cur-y)))
                (< 0 (move 'E)))
           (go)]
          [(and (eq? cur-dir 'S)
                (not (hash-has-key? map (w cur-x cur-y)))
                (< 0 (move 'W)))
           (go)]
          [(and (eq? cur-dir 'E)
                (not (hash-has-key? map (s cur-x cur-y)))
                (< 0 (move 'S)))
           (go)]
          [(and (eq? cur-dir 'W)
                (not (hash-has-key? map (n cur-x cur-y)))
                (< 0 (move 'N)))
           (go)]
          ;; explore left
          [(and (eq? cur-dir 'N)
                (not (hash-has-key? map (w cur-x cur-y)))
                (< 0 (move 'W)))
           (go)]
          [(and (eq? cur-dir 'S)
                (not (hash-has-key? map (e cur-x cur-y)))
                (< 0 (move 'E)))
           (go)]
          [(and (eq? cur-dir 'E)
                (not (hash-has-key? map (n cur-x cur-y)))
                (< 0 (move 'N)))
           (go)]
          [(and (eq? cur-dir 'W)
                (not (hash-has-key? map (s cur-x cur-y)))
                (< 0 (move 'S)))
           (go)]
          ;; backtrack forward
          [(and (eq? cur-dir 'N)
                (< 0 (move 'N)))
           (go)]
          [(and (eq? cur-dir 'S)
                (< 0 (move 'S)))
           (go)]
          [(and (eq? cur-dir 'E)
                (< 0 (move 'E)))
           (go)]
          [(and (eq? cur-dir 'W)
                (< 0 (move 'W)))
           (go)]
          ;; backtrack turn
          [(and (not (eq? cur-dir 'N))
                (< 0 (move 'N)))
           (go)]
          [(and (not (eq? cur-dir 'S))
                (< 0 (move 'S)))
           (go)]
          [(and (not (eq? cur-dir 'E))
                (< 0 (move 'E)))
           (go)]
          [(and (not (eq? cur-dir 'W))
                (< 0 (move 'W)))
           (go)]
          [else (raise (error "don't know where to go next!"))]))
      (go))


    ;; simple area search
    (define cur-phase 'find-edge)
    (define/public (search)
      (do ([r (move cur-dir) (move cur-dir)])
          ((eq? r 2) (+ cur-x cur-y))
        (displayln (format "~a ~a ~a,~a: ~a" cur-dir cur-phase cur-x cur-y r))
        (if (eq? r 0)
            (case cur-phase
              [(find-edge) (displayln "finding corner") (set! cur-dir 'E) (set! cur-phase 'find-corner)]
              [(find-corner) (set! cur-dir 'S) (set! cur-phase 'comb-s)]
              [(comb-s) (set! cur-dir 'W) (set! cur-phase 'shift-n)]
              [(shift-n) (set! cur-dir 'N) (set! cur-phase 'comb-n)]
              [(comb-n) (set! cur-dir 'W) (set! cur-phase 'shift-s)]
              [(shift-s) (set! cur-dir 'S) (set! cur-phase 'comb-s)])
            (case cur-phase
              [(shift-n) (set! cur-dir 'N) (set! cur-phase 'comb-n)]
              [(shift-s) (set! cur-dir 'S) (set! cur-phase 'comb-s)]))
        (sleep 0.1)))))

(module+ main
  (send (new repair-droid% [program (open-input-file "day15.input.txt")])
        map-search))
