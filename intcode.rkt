#lang racket

(require rackunit)

(provide intcode%)

(define (int->list n len)
  (map (λ (x) (remainder (quotient n (expt 10 x)) 10)) (range len)))

(module+ test
  (check-equal? (int->list 1234 4) '(4 3 2 1))
  (check-equal? (int->list 4 4) '(4 0 0 0)))

(define intcode%
  (class object%
    (init program output)
    (define source (cond [(list? program) program]
                         [(string? program) (read-program (open-input-string program))]
                         [(port? program) (read-program program)]))
    (define mem (make-hash (map cons (range (length source)) source)))
    (define out output)
    (define rel-base 0)

    (super-new)

    (define/private (read-program ip)
      (map string->number (string-split (read-line ip) ",")))
    (define/public (peek addr)
      (if (hash-has-key? mem addr)
          (hash-ref mem addr)
          0))
    (define/public (poke! addr v)
      (hash-set! mem addr v))
    (define/private (pval mode arg)
      (case mode
        [(0) (peek arg)]
        [(1) arg]
        [(2) (peek (+ rel-base arg))]))
    (define/private (paddr mode arg)
      (case mode
        [(0) arg]
        [(2) (+ rel-base arg)]))
    (define/private (op-binary op addr modes)
      (poke! (paddr (third modes) (peek (+ 3 addr)))
             (op (pval (first modes) (peek (add1 addr)))
                 (pval (second modes) (peek (+ 2 addr))))))
    (define/private (op-input addr modes)
      (cond
        [(thread? out) (thread-send out -99)]
        [(channel? out) (channel-put out -99)]
        [(port? out) (display "> " out)])
      (poke! (paddr (first modes) (peek (add1 addr)))
             (cond
               [(thread? out) (thread-receive)]
               [(channel? out) (channel-get out)]
               [(port? out) (read (current-input-port))])))
    (define/private (op-output addr modes)
      (let ([v (pval (first modes) (peek (add1 addr)))])
        (cond
          [(thread? out) (thread-send out v)]
          [(channel? out) (channel-put out v)]
          [(port? out) (displayln v out)])))
    (define/private (op-jump f addr modes)
      (if (f (pval (first modes) (peek (add1 addr))))
          (pval (second modes) (peek (+ addr 2)))
          (+ addr 3)))
    (define/private (op-cmp f addr modes)
      (poke! (paddr (third modes) (peek (+ addr 3)))
             (if (f (pval (first modes) (peek (add1 addr)))
                    (pval (second modes) (peek (+ 2 addr))))
                 1
                 0)))
    (define/private (op-rel-base addr modes)
      (set! rel-base (+ rel-base (pval (first modes) (peek (add1 addr))))))
    (define/private (mem->list)
      (hash-map mem (λ (_ v) v) #t))
    (define/public (exe addr)
      (with-handlers
        ([exn:fail? (λ (e) (displayln (format "got exception during intcode execution:\n~a\nsource: ~a\naddress: ~a" e (mem->list) addr)))])
        (let* ([opmodes (peek addr)]
               [opcode (remainder opmodes 100)]
               [modecode (quotient opmodes 100)]
               [modes (int->list modecode 3)])
          (case opcode
            [(99) (mem->list)]
            [(1) (op-binary + addr modes) (exe (+ addr 4))]
            [(2) (op-binary * addr modes) (exe (+ addr 4))]
            [(3) (op-input addr modes) (exe (+ addr 2))]
            [(4) (op-output addr modes) (exe (+ addr 2))]
            [(5) (exe (op-jump (λ (x) (not (eq? x 0))) addr modes))]
            [(6) (exe (op-jump (λ (x) (eq? x 0)) addr modes))]
            [(7) (op-cmp < addr modes) (exe (+ addr 4))]
            [(8) (op-cmp = addr modes) (exe (+ addr 4))]
            [(9) (op-rel-base addr modes) (exe (+ addr 2))]))))

    (define/private (pval/asm mode arg)
      (case mode
        [(0) (format "@~a" arg)]
        [(1) arg]
        [(2) (format "*~a" arg)]))
    (define/private (op-binary/asm op addr modes)
      (format "~a: ~a ~a,~a ~a"
              addr
              op
              (pval/asm (first modes) (peek (add1 addr)))
              (pval/asm (second modes) (peek (+ 2 addr)))
              (pval/asm (third modes) (peek (+ 3 addr)))))
    (define/private (op-input/asm addr modes)
      (format "~a: INP ~a" addr (pval/asm (first modes) (peek (add1 addr)))))
    (define/private (op-output/asm addr modes)
      (format "~a: OUT ~a" addr (pval/asm (first modes) (peek (add1 addr)))))
    (define/private (op-jump/asm f addr modes)
      (format "~a: ~a ~a ~a" addr f
              (pval/asm (first modes) (peek (add1 addr)))
              (pval/asm (second modes) (peek (+ addr 2)))))
    (define/private (op-cmp/asm f addr modes)
      (format "~a: ~a ~a,~a ~a"
              addr
              f
              (pval/asm (first modes) (peek (add1 addr)))
              (pval/asm (second modes) (peek (+ 2 addr)))
              (pval/asm (third modes) (peek (+ 3 addr)))))
    (define/private (op-rel-base/asm addr modes)
      (format "~a: REL ~a" addr (pval/asm (first modes) (peek (add1 addr)))))
    (define/public (asm addr)
      (let* ([opmodes (peek addr)]
             [opcode (remainder opmodes 100)]
             [modecode (quotient opmodes 100)]
             [modes (int->list modecode 3)])
        (case opcode
          [(99) (cons (format "~a: END" addr) (asm (+ addr 1)))]
          [(1) (cons (op-binary/asm "ADD" addr modes) (asm (+ addr 4)))]
          [(2) (cons (op-binary/asm "MUL" addr modes) (asm (+ addr 4)))]
          [(3) (cons (op-input/asm addr modes) (asm (+ addr 2)))]
          [(4) (cons (op-output/asm addr modes) (asm (+ addr 2)))]
          [(5) (cons (op-jump/asm "JNZ" addr modes) (asm (+ addr 3)))]
          [(6) (cons (op-jump/asm "JZ " addr modes) (asm (+ addr 3)))]
          [(7) (cons (op-cmp/asm "LT " addr modes) (asm (+ addr 4)))]
          [(8) (cons (op-cmp/asm "EQ " addr modes) (asm (+ addr 4)))]
          [(9) (cons (op-rel-base/asm addr modes) (asm (+ addr 2)))]
          [else (cons (format "~a: DAT ~a" addr (peek addr))
                      (if (< addr (apply max (hash-keys mem)))
                          (asm (+ addr 1))
                          '()))])))))

(module+ main
  (define assembly-mode (make-parameter #f))
  (define filename (command-line
                    #:program "intcode"
                    #:once-each
                    [("-a" "--assembly") "Output asm-esque version of program"
                                         (assembly-mode #t)]
                    #:args (filename)
                    filename))
  (define program (open-input-file filename))
  (define interp (new intcode%
                      [output (current-output-port)]
                      [program program]))
  (cond
    [(assembly-mode) (for-each displayln (send interp asm 0))]
    [else (send interp exe 0)]))
