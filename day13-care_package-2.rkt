#lang racket

(require racket/gui
         pict
         "intcode.rkt")

;; empty rectangle in case we want to draw something there later
(define (draw-empty-tile dc dx dy width height [color "green"])
  (define old-brush (send dc get-brush))
  (define old-pen (send dc get-pen))
  (send dc set-brush
        (new brush% [style 'solid]
             [color color]))
  (send dc set-pen
        (new pen% [style 'solid]
             [color color]))
  (send dc draw-rectangle dx dy width height)
  (send dc set-brush old-brush)
  (send dc set-pen old-pen))

(define (draw-wall-tile dc dx dy width height [color "blue"])
  (define old-brush (send dc get-brush))
  (define old-pen (send dc get-pen))
  (send dc set-brush
        (new brush% [style 'solid]
             [color color]))
  (send dc set-pen
        (new pen% [width 3] [color "gray"]))
  (send dc draw-rectangle dx dy width height)
  (send dc set-brush old-brush)
  (send dc set-pen old-pen))

(define (draw-block-tile dc dx dy width height [color "purple"] [style 'crossdiag-hatch])
  (define old-brush (send dc get-brush))
  (define old-pen (send dc get-pen))
  (send dc set-brush
        (new brush% [style style]
             [color color]))
  (send dc set-pen
        (new pen% [width 3] [color color]))
  (send dc draw-rounded-rectangle dx dy width height (/ (min width height) 4))
  (send dc set-brush old-brush)
  (send dc set-pen old-pen))

(define (draw-paddle-tile dc dx dy width height [color "black"] [style 'vertical-hatch])
  (define old-brush (send dc get-brush))
  (define old-pen (send dc get-pen))
  (send dc set-brush
        (new brush% [style style]
             [color color]))
  (send dc set-pen
        (new pen% [width 3] [color color]))
  (send dc draw-rectangle dx dy width (/ height 3))
  (send dc set-brush old-brush)
  (send dc set-pen old-pen))

(define (draw-ball-tile dc dx dy width height [color "pink"] [style 'solid])
  (define old-brush (send dc get-brush))
  (define old-pen (send dc get-pen))
  (send dc set-brush
        (new brush% [style style]
             [color color]))
  (send dc set-pen
        (new pen% [width 2] [color color]))
  (send dc draw-ellipse dx dy width height)
  (send dc set-brush old-brush)
  (send dc set-pen old-pen))

(define tiles (vector
               draw-empty-tile
               draw-wall-tile
               draw-block-tile
               draw-paddle-tile
               draw-ball-tile))

(define (draw-tile dc dx dy width height num)
  (when (< -1 num (vector-length tiles))
      ((vector-ref tiles num) dc dx dy width height)))

;; this could be used to actually control the game, but easier to let it control itself
(define game-canvas%
  (class canvas%
    (define dir 0)
    (define/public (get-dir) dir)
    (define/override (on-event event)
      (cond
        [(send event get-left-down) (set! dir -1)]
        [(send event get-right-down) (set! dir 1)]
        [else (set! dir 0)])
      (send msg set-label (format "~a" dir)))
    #;(define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    (super-new)))

(define (game program canvas [block-size 10])
  (define ch (make-channel))
  (define brain (thread (λ ()
                          (define prog (new intcode% [program program] [output ch]))
                          (send prog poke! 0 2)
                          (send prog exe 0))))
  (define board (make-hash))
  (do ([ball-x 0]
       [paddle-x 0]
       [x (channel-get ch) (sync ch (thread-dead-evt brain))])
      ((thread-dead? brain))
    (if (thread-dead? brain)
        0
        (begin
          ;(displayln (format "~a ~a ~a" x y tile-num))
          (cond
            [(eq? x -1)
             (define y (sync ch (thread-dead-evt brain)))
             (define tile-num (sync ch (thread-dead-evt brain)))
             (send msg set-label (format "Score: ~a" tile-num))]
            [(eq? x -99)
             (send canvas flush)
             ;(sleep 1)
             (channel-put ch (cond
                               [(< ball-x paddle-x) -1]
                               [(> ball-x paddle-x) 1]
                               [else 0]))]
            [else
             (define y (sync ch (thread-dead-evt brain)))
             (define tile-num (sync ch (thread-dead-evt brain)))
             (when (eq? tile-num 3) (set! paddle-x x))
             (when (eq? tile-num 4) (set! ball-x x))
             (draw-tile (send canvas get-dc) (* x block-size) (* y block-size) block-size block-size tile-num)
             (hash-set! board (vector x y) tile-num)])))))

(define frame (new frame% [label "BUSTER"]
                   [width 600]
                   [height 600]))

(define msg (new message% [parent frame]
                 [label "Score: 000"]))

(module+ main
  (send frame show #t)
  (thread (λ () (game (open-input-file "day13.input.txt")
                      (new canvas% [parent frame])
                      15))))
