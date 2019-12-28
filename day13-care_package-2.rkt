#lang racket

(require racket/gui
         pict
         "intcode.rkt")

;; empty rectangle in case we want to draw something there later
(define (draw-empty-tile dc dx dy width height [color "snow"])
  (define old-brush (send dc get-brush))
  (define old-pen (send dc get-pen))
  (send dc set-brush
        (new brush% [style 'transparent]
             [color color]))
  (send dc set-pen
        (new pen% [style 'transparent]))
  (send dc draw-rectangle dx dy width height)
  (send dc set-brush old-brush)
  (send dc set-pen old-pen))

(define (draw-wall-tile dc dx dy width height [color "black"])
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

(define (draw-block-tile dc dx dy width height [color "black"] [style 'crossdiag-hatch])
  (define old-brush (send dc get-brush))
  (define old-pen (send dc get-pen))
  (send dc set-brush
        (new brush% [style style]
             [color color]))
  (send dc set-pen
        (new pen% [width 3] [color color]))
  (send dc draw-rounded-rectangle dx dy width height (/ (min width height) 2))
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

(define (draw-ball-tile dc dx dy width height [color "white"] [style 'solid])
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
  (when (< 0 num (vector-length tiles))
      ((vector-ref tiles num) dc dx dy width height)))

(define (draw-board canvas dc)
  (draw-tile dc 100 100 30 30 3))

(define game-canvas%
  (class canvas%
    #;
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    (super-new)))

(define output-ch (make-channel))
(define game (exe (read-program (open-input-file "day13.input.txt")) 0 output-ch))

#;
(define (game program)
  (let* ([ch (make-channel)]
         [brain (thread (λ () (exe (read-program program) 0 ch)))]
         [board (make-hash)])
    (define (run board)
      (define x (sync ch (thread-dead-evt brain)))
      (define y (sync ch (thread-dead-evt brain)))
      (define tile-num (sync ch (thread-dead-evt brain)))
      (if (thread-dead? brain)
          #f
          (run (hash-set board (vector x y) tile-num))))
    (run (make-immutable-hash))))

(define frame (new frame% [label "Blockbreaker"]
                   [width 600]
                   [height 600]))

(define msg (new message% [parent frame]
                 [label "Score: 000"]))

(new game-canvas% [parent frame]
     [paint-callback draw-board]
     [program (open-input-file "day13.input.txt")])

(define current-game (game (open-input-file "day12.input.txt")))

(module+ main
  (send frame show #t))
