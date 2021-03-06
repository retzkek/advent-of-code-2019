#lang racket

(require rackunit)

(provide read-program
         exe)

(define (int->list n len)
  (map (λ (x) (remainder (quotient n (expt 10 x)) 10)) (range len)))

(module+ test
  (check-equal? (int->list 1234 4) '(4 3 2 1))
  (check-equal? (int->list 4 4) '(4 0 0 0)))


(struct intcode
  (program rel-base ext-mem)
  #:transparent)

(define (mem-ref prog address)
  (let* ([p (intcode-program prog)])
    (if (> address (length p))
        (if (hash-has-key? (intcode-ext-mem prog) address)
            (hash-ref (intcode-ext-mem prog) address)
            0)
        (list-ref p address))))

(module+ test
  (let ([p (intcode '(1 2 3) 0 (make-hash '((99 . 4))))])
    (check-eq? (mem-ref p 0) 1)
    (check-eq? (mem-ref p 1) 2)
    (check-eq? (mem-ref p 2) 3)
    (check-eq? (mem-ref p 99) 4)))

(define (mem-set prog address val)
  (let* ([p (intcode-program prog)])
    (if (> address (length p))
        (struct-copy intcode prog [ext-mem (hash-set (intcode-ext-mem prog) address val)])
        (struct-copy intcode prog [program (append (take p address)
                                                   (list val)
                                                   (drop p (+ address 1)))]))))

(module+ test
  (let ([p (intcode '(1 2 3) 0 (make-immutable-hash))])
    (check-equal? (intcode-program (mem-set p 0 9)) '(9 2 3))
    (check-equal? (intcode-program (mem-set p 1 9)) '(1 9 3))
    (check-equal? (intcode-program (mem-set p 2 9)) '(1 2 9))
    (check-equal? (intcode-ext-mem (mem-set p 99 9)) (make-immutable-hash '((99 . 9))))))

(define (param-value mode arg prog)
  (case mode
    [(0) (mem-ref prog arg)]
    [(1) arg]
    [(2) (mem-ref prog (+ (intcode-rel-base prog) arg))]))

(define (param-address mode arg prog)
  (case mode
    [(0) arg]
    [(2) (+ (intcode-rel-base prog) arg)]))

(define-syntax-rule (op2 op prog args step modes out)
  (let* ([a (param-value (first modes) (second args) prog)]
         [b (param-value (second modes) (third args) prog)]
         [dest (param-address (third modes) (fourth args) prog)])
    (exe (mem-set prog dest (op a b))
         (+ step 4)
         out)))

(define (op-input prog args step modes out)
  #;(display "> ")
  (let ([v (thread-receive)]
        [dest (param-address (first modes) (second args) prog)])
    (exe (mem-set prog dest v)
         (+ step 2)
         out)))

(define (op-output prog args step modes out)
  (let ([v (param-value (first modes) (second args) prog)])
    (cond
      [(thread? out) (thread-send out v)]
      [(channel? out) (channel-put out v)]
      [(port? out) (displayln v out)]))
  (exe prog (+ step 2) out))

(define (op-jump f prog args step modes out)
  (exe prog (if (f (param-value (first modes) (second args) prog))
                (param-value (second modes) (third args) prog)
                (+ step 3))
       out))

(define (op-cmp f prog args step modes out)
  (let* ([a (param-value (first modes) (second args) prog)]
         [b (param-value (second modes) (third args) prog)]
         [val (if (f a b) 1 0)]
         [dest (param-address (third modes) (fourth args) prog)])
    (exe (mem-set prog dest val) (+ step 4) out)))

(define (op-rel-base prog args step modes out)
  (exe (struct-copy intcode prog [rel-base (+ (intcode-rel-base prog)
                                              (param-value (first modes)
                                                           (second args)
                                                           prog))])
       (+ step 2)
       out))

(define (exe prog step out)
  (with-handlers
    ([exn:fail? (λ (e) (displayln (format "got exception during intcode execution:\n~a\nprog: ~a\nstep: ~a\nprog at step: ~a" e prog step (drop (intcode-program prog) step))))])
    (let* ([args (drop (intcode-program prog) step)]
          [opmodes (first args)]
          [opcode (remainder opmodes 100)]
          [modecode (quotient opmodes 100)]
          [modes (int->list modecode 3)])
     (case opcode
       [(99) prog]
       [(1) (op2 + prog args step modes out)]
       [(2) (op2 * prog args step modes out)]
       [(3) (op-input prog args step modes out)]
       [(4) (op-output prog args step modes out)]
       [(5) (op-jump (λ (x) (not (eq? x 0))) prog args step modes out)]
       [(6) (op-jump (λ (x) (eq? x 0)) prog args step modes out)]
       [(7) (op-cmp < prog args step modes out)]
       [(8) (op-cmp = prog args step modes out)]
       [(9) (op-rel-base prog args step modes out)]))))

(define (read-program ip)
  (intcode (map string->number (string-split (read-line ip) ",")) 0 (make-immutable-hash)))

(module+ test
  (check-equal? (intcode-program (read-program (open-input-string "1,2,3"))) '(1 2 3)))

(define (check-program prog newprog)
  (check-equal?
   (intcode-program (exe (read-program (open-input-string prog)) 0 (make-channel)))
   (intcode-program (read-program (open-input-string newprog)))))

(module+ test
  (check-program "1,0,0,0,99" "2,0,0,0,99")
  (check-program "2,3,0,3,99" "2,3,0,6,99")
  (check-program "2,4,4,5,99,0" "2,4,4,5,99,9801")
  (check-program "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"))

(define (check-program-io program in out)
  (let* ([ch (make-channel)]
        [worker (thread (λ () (exe (read-program (open-input-string program)) 0 ch)))])
    (when in (thread-send worker in))
    (if (list? out)
        (for ([o (in-list out)])
          (check-equal? (channel-get ch) o))
        (check-equal? (channel-get ch) out))
    (thread-wait worker)))

(module+ test
  (check-program-io "3,9,8,9,10,9,4,9,99,-1,8" 8 1)
  (check-program-io "3,9,8,9,10,9,4,9,99,-1,8" 2 0)
  (check-program-io "3,9,7,9,10,9,4,9,99,-1,8" 2 1)
  (check-program-io "3,9,7,9,10,9,4,9,99,-1,8" 10 0)
  (check-program-io "3,3,1108,-1,8,3,4,3,99" 8 1)
  (check-program-io "3,3,1108,-1,8,3,4,3,99" 2 0)
  (check-program-io "3,3,1107,-1,8,3,4,3,99" 2 1)
  (check-program-io "3,3,1107,-1,8,3,4,3,99" 10 0)
  (check-program-io "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 0 0)
  (check-program-io "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 22 1)
  (check-program-io "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 0 0)
  (check-program-io "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 22 1)
  (check-program-io "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                    2 999)
  (check-program-io "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                    8 1000)
  (check-program-io "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                    9 1001)
  (check-program-io "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
                    #f
                    '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99))
  (check-program-io "1102,34915192,34915192,7,4,7,99,0"
                    #f
                    1219070632396864)
  (check-program-io "104,1125899906842624,99" #f 1125899906842624))

(define (run-intcode prog [in #f])
  (let* ([worker (thread (λ () (exe prog 0 (current-output-port))))])
    (when in (thread-send worker in))
    (thread-wait worker)))

(module+ main
  (let ([program (read-program
                  (open-input-file
                   (command-line
                    #:program "intcode"
                    #:args (filename)
                    filename)))])
    (run-intcode program (read))))
