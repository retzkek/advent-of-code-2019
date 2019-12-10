#lang racket

(require rackunit)

(provide read-program
         exe)


(define (val-at lst pos)
  (first (drop lst pos)))

(module+ test
  (check-eq? (val-at '(1 2 3) 0) 1)
  (check-eq? (val-at '(1 2 3) 1) 2)
  (check-eq? (val-at '(1 2 3) 2) 3))

(define (sub-at lst pos val)
  (append (take lst pos) (list val) (drop lst (+ pos 1))))

(module+ test
  (check-equal? (sub-at '(1 2 3) 0 9) '(9 2 3))
  (check-equal? (sub-at '(1 2 3) 1 9) '(1 9 3))
  (check-equal? (sub-at '(1 2 3) 2 9) '(1 2 9)))

(define (int->list n len)
  (map (λ (x) (remainder (quotient n (expt 10 x)) 10)) (range len)))

(module+ test
  (check-equal? (int->list 1234 4) '(4 3 2 1))
  (check-equal? (int->list 4 4) '(4 0 0 0)))


(struct intcode
  (program rel-base ext-mem)
  #:transparent)

(define-syntax-rule (param mode arg prog)
  (if (eq? mode 0)
      (val-at (intcode-program prog) arg)
      arg))

(define-syntax-rule (op2 op prog args step modes out)
  (let* ([a (param (first modes) (second args) prog)]
         [b (param (second modes) (third args) prog)]
         [dest (fourth args)])
    (exe (struct-copy intcode prog [program (sub-at (intcode-program prog) dest (op a b))])
         (+ step 4)
         out)))

(define (op-input prog args step modes out)
  #;(display "> ")
  (let ([v (thread-receive)])
    (exe (struct-copy intcode prog
                      [program (sub-at (intcode-program prog) (second args) v) ])
         (+ step 2)
         out)))

(define (op-output prog args step modes out)
  (let ([v (param (first modes) (second args) prog)])
    (cond
      [(thread? out) (thread-send out v)]
      [(channel? out) (channel-put out v)]))
  (exe prog (+ step 2) out))

(define (op-jump f prog args step modes out)
  (exe prog (if (f (param (first modes) (second args) prog))
                (param (second modes) (third args) prog)
                (+ step 3))
       out))

(define (op-cmp f prog args step modes out)
  (let* ([a (param (first modes) (second args) prog)]
         [b (param (second modes) (third args) prog)]
         [val (if (f a b) 1 0)]
         [dest (fourth args)])
    (exe (struct-copy intcode prog [program (sub-at (intcode-program prog) dest val)]) (+ step 4) out)))

(define (op-rel-base f prog args step modes out)
  (exe (struct-copy intcode prog [rel-base (+ (intcode-rel-base prog)
                                              (param (first modes)
                                                     (second args)
                                                     prog))])
       (+ step 2)
       out))

(define (exe prog step out)
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
      [(9) (op-rel-base prog args step modes out)])))

(define (read-program ip)
  (intcode (map string->number (string-split (read-line ip) ",")) 0 (make-hash)))

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
    (thread-send worker in)
    (check-equal? (channel-get ch) out)
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
                    9 1001))

(define (make-amps program out phases [amps '()])
  (let ([th (thread (lambda () (exe program 0 out)))])
    (thread-send th (first phases))
    (if (= 1 (length phases))
        (cons th amps)
        (make-amps program th (rest phases) (cons th amps)))))

(define (feedback-amps program phases)
  (let* ([oc (make-channel)]
         [rc (make-channel)]
         [amps (make-amps (read-program (open-input-string program)) oc (reverse phases))]
         [feedback (thread (lambda ()
                             (let loop ()
                               (let ([v (channel-get oc)])
                                 #;(displayln (format "oc: ~a" v))
                                 (if (thread-dead? (first amps))
                                                   (channel-put rc v)
                                                   (thread-send (first amps) v))
                               (loop)))))])
    ;(displayln (length amps))
    (thread-send (first amps) 0)
    (for-each thread-wait amps)
    (channel-get rc)))

(module+ test
  (check-eq? (feedback-amps "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
                            '(9 8 7 6 5))
             139629729)
  (check-eq? (feedback-amps "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
                            '(9 7 8 5 6))
             18216))

(define (max-phases program)
  (argmax (λ (phases) (feedback-amps program phases)) (permutations (range 5 10))))

(module+ test
  (check-equal? (max-phases "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
                '(9 8 7 6 5))
  (check-equal? (max-phases "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
                '(9 7 8 5 6)))

(module+ main
  (let ([program (read-line
                  (open-input-file
                   (command-line
                    #:program "intcode"
                    #:args (filename)
                    filename)))])
    (displayln (feedback-amps program (max-phases program)))))
