#lang racket

(provide day)

(require threading)
(require "./input.rkt")
(require "./util.rkt")

(define (parse-instruction instr)
  (define rest (substring instr 1))
  (define n (string->number rest))
  (case (string-ref instr 0)
    [(#\L) (- n)]
    [(#\R) (+ n)]
    [else (error 'parse-instruction (string-append "failed to parse instrunction " instr))]))

(define (apply-instruction instr cursor)
  (modulo (+ instr cursor) 100))

(define parse
  (λ~> (map string-trim _)
       (filter non-empty-string? _)
       (map parse-instruction _)))

(define-day day 1 'real
  (λ (port)
    (define instrs (~> port
                       read-lines
                       parse))
    (define part1 (~> instrs (scan apply-instruction 50 _) (count zero? _)))

    (define part2
      (let go ([acc 0] [cursor 50] [instrs instrs])
        (match instrs
          ['() acc]
          [(cons 0 instrs)
           (go acc cursor instrs)]
          [(cons instr instrs)
           (let* ([dir (sign instr)]
                  [v (modulo (+ cursor dir) 100)])
             (go (if (= v 0) (+ 1 acc) acc)
                 v
                 (cons (- instr dir) instrs)))])))
    
    `(,part1 ,part2)))
