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

(define-syntax-rule (op2 op prog args step)
  (let ([a (second args)]
        [b (third args)]
        [dest (fourth args)])
    (exe (sub-at prog dest (op (val-at prog a) (val-at prog b))) (+ step 4))))

(define (exe prog step)
  (let* ([args (drop prog step)]
         [opcode (first args)])
    (cond
      [(eq? opcode 99) prog]
      [(eq? opcode 1) (op2 + prog args step)]
       [(eq? opcode 2) (op2 * prog args step)])))

(module+ test
  (check-equal? (exe '(1 4 5 4 11 88) 0) '(1 4 5 4 99 88))
  (check-equal? (exe '(2 4 5 4 3 33) 0) '(2 4 5 4 99 33)))

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
  (display (exe (append '(1 12 2) (drop (read-program (current-input-port)) 3)) 0)))
                 