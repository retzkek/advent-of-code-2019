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

(define-syntax-rule (param mode arg prog)
  (if (eq? mode 0)
      (val-at prog arg)
      arg))

(define-syntax-rule (op2 op prog args step modes)
  (let* ([a (param (first modes) (second args) prog)]
         [b (param (second modes) (third args) prog)]
         [dest (fourth args)])
    (exe (sub-at prog dest (op a b)) (+ step 4))))

(define (op-input prog args step modes)
  (display "> ")
  (exe (sub-at prog (second args) (read)) (+ step 2)))

(define (op-output prog args step modes)
  (let* ([val (param (first modes) (second args) prog)])
    (writeln val)
    (exe prog (+ step 2))))

(define (exe prog step)
  (let* ([args (drop prog step)]
         [opmodes (first args)]
         [opcode (remainder opmodes 100)]
         [modecode (quotient opmodes 100)]
         [modes (int->list modecode 3)])
    (cond
      [(eq? opcode 99) prog]
      [(eq? opcode 1) (op2 + prog args step modes)]
      [(eq? opcode 2) (op2 * prog args step modes)]
      [(eq? opcode 3) (op-input prog args step modes)]
      [(eq? opcode 4) (op-output prog args step modes)])))

(module+ test
  (check-equal? (exe '(1 4 5 4 11 88) 0) '(1 4 5 4 99 88))
  (check-equal? (exe '(2 4 5 4 3 33) 0) '(2 4 5 4 99 33))
  (parameterize ([current-input-port (open-input-string "23")])
    (check-equal? (exe '(3 3 99 0) 0) '(3 3 99 23)))
  (let ([out (open-output-string)])
    (parameterize ([current-output-port out])
      (check-equal? (exe '(4 3 99 23) 0) '(4 3 99 23))
      (check-equal? (get-output-string out) "23\n")))
  (check-equal? (exe '(1002 4 3 4 33) 0) '(1002 4 3 4 99)))

(define (read-program ip)
  (map string->number (string-split (read-line ip) ",")))

(module+ test
  (check-equal? (read-program (open-input-string "1,2,3")) '(1 2 3)))

(define-syntax-rule (check-program prog newprog)
  (check-equal?
   (exe (read-program (open-input-string prog)) 0)
   (read-program (open-input-string newprog))))

(module+ test
  (check-program "1,0,0,0,99" "2,0,0,0,99")
  (check-program "2,3,0,3,99" "2,3,0,6,99")
  (check-program "2,4,4,5,99,0" "2,4,4,5,99,9801")
  (check-program "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"))

(module+ main
  (display
   (exe
    (read-program
     (open-input-file
      (command-line
       #:program "intcode"
       #:args (filename)
       filename)))
    0)))
