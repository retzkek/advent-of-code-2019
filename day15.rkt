#lang racket

(require "intcode.rkt"
         charterm)

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

    (define droid-map (make-hash))
    (define o2 9999)
    (define/private (draw-map)
      (for* ([y (in-range max-y (sub1 min-y) -1)]
             [x (in-range min-x (add1 max-x))])
        (display
         (cond
           [(and (eq? x cur-x) (eq? y cur-y)) "@"]
           [(hash-has-key? droid-map (cons x y))
            (case (get-map x y)
              ((-1) "#")
              ((o2) "$")
              (else (get-map x y)))]
           [else " "]))
        (when (eq? x max-x) (newline))))
    (define/private (set-map! x y v)
      (hash-set! droid-map (cons x y) v)
      (when (> y max-y) (set! max-y y))
      (when (< y min-y) (set! min-y y))
      (when (> x max-x) (set! max-x x))
      (when (< x min-x) (set! min-x x))
      v)
    (define/private (get-map x y)
      (define key (cons x y))
      (if (hash-has-key? droid-map key)
          (hash-ref droid-map key)
          #f))
    (define/private (mv dir x y)
      (case dir
        ((N) (cons x (add1 y)))
        ((S) (cons x (sub1 y)))
        ((E) (cons (add1 x) y))
        ((W) (cons (sub1 x) y))
        (else (cons x y))))

    (define/private (move dir)
      (when (not (eq? -99 (sync/timeout 10 ch)))
        (raise (error "expected prompt")))
      (channel-put ch (hash-ref dirs dir))
      (define r (channel-get ch))
      (define-values (next-x next-y)
        (case dir
          ((N) (values cur-x (add1 cur-y)))
          ((S) (values cur-x (sub1 cur-y)))
          ((W) (values (sub1 cur-x) cur-y))
          ((E) (values (add1 cur-x) cur-y))))
      (case r
        ((0) (set-map! next-x next-y -1))
        ((1 2) (set! cur-dir dir)
             (set! cur-x next-x)
             (set! cur-y next-y)
             (set-map! cur-x cur-y
                       (if (get-map cur-x cur-y)
                           (add1 (get-map cur-x cur-y))
                           (if (eq? r 2) o2 1)))
             (set! cur-z (get-map cur-x cur-y))
             cur-z)))

    (define/private (maybe-explore? check-dir go-dir)
      (and (eq? cur-dir check-dir)
           (not (hash-has-key? droid-map (mv go-dir cur-x cur-y)))
           (> 0 (move go-dir))))
    (define/private (maybe-backtrack? check-dir go-dir)
      (and (eq? cur-dir check-dir)
           (> 0 (move go-dir))))

    ;; manual search
    (define/public (manual-search)
      (with-charterm
          (do ([key #\a (charterm-read-key)])
              ((or (eq? key `escape) (eq? key #\q)))
            (charterm-clear-screen)
            (charterm-cursor 0 0)
            (charterm-inverse)
            (charterm-display "At ")
            (charterm-display cur-x #:width 5)
            (charterm-display cur-y #:width 5)
            (charterm-display "Facing ")
            (charterm-display cur-dir)
            (charterm-normal)
            (charterm-cursor 1 1)
            (hash-for-each
             droid-map
             (λ (loc v)
               (charterm-cursor (- (car loc) min-x -1)
                                (- max-y (cdr loc) -2))
               (charterm-display
                (cond
                  [(= (car loc) (cdr loc) 0) "O"]
                  [(and (= (car loc) cur-x) (= (cdr loc) cur-y)) "@"]
                  [(< v 0) "#"]
                  [(= v o2) "$"]
                  [else v]))))
            (case key
              ((up #\k) (move 'N))
              ((down #\j) (move 'S))
              ((right #\l) (move 'E))
              ((left #\h) (move 'W))))))

    ;; mapping-based search
    (define/public (map-search)
      (define (go)
        ;(hash-set! droid-map (cons cur-x cur-y) cur-z)
        (displayln (format "~a ~a ~a ~a" cur-dir cur-x cur-y cur-z))
        (draw-map)
        (sleep 0.1)
        (cond
          [(eq? 9999 (get-map cur-x cur-y)) (cons cur-x cur-y)]
          ;; explore forward
          [(maybe-explore? 'N 'N) (go)]
          [(maybe-explore? 'S 'S) (go)]
          [(maybe-explore? 'W 'W) (go)]
          [(maybe-explore? 'E 'E) (go)]
          ;; explore right
          [(maybe-explore? 'N 'E) (go)]
          [(maybe-explore? 'S 'W) (go)]
          [(maybe-explore? 'E 'S) (go)]
          [(maybe-explore? 'W 'N) (go)]
          ;; explore left
          [(maybe-explore? 'N 'W) (go)]
          [(maybe-explore? 'S 'E) (go)]
          [(maybe-explore? 'E 'N) (go)]
          [(maybe-explore? 'W 'S) (go)]
          ;; backtrack forward
          [(maybe-backtrack? 'N 'N) (go)]
          [(maybe-backtrack? 'S 'S) (go)]
          [(maybe-backtrack? 'W 'W) (go)]
          [(maybe-backtrack? 'E 'E) (go)]
          ;; backtrack right
          [(maybe-backtrack? 'N 'E) (go)]
          [(maybe-backtrack? 'S 'W) (go)]
          [(maybe-backtrack? 'W 'N) (go)]
          [(maybe-backtrack? 'E 'S) (go)]
          ;; backtrack left
          [(maybe-backtrack? 'N 'W) (go)]
          [(maybe-backtrack? 'S 'E) (go)]
          [(maybe-backtrack? 'W 'S) (go)]
          [(maybe-backtrack? 'E 'N) (go)]
          ;; backtrack back
          [(maybe-backtrack? 'N 'S) (go)]
          [(maybe-backtrack? 'S 'N) (go)]
          [(maybe-backtrack? 'W 'E) (go)]
          [(maybe-backtrack? 'E 'W) (go)]
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
        manual-search))
