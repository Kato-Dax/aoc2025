#lang racket
(require threading)

(require
  (prefix-in day1: "./day1.rkt")
  (prefix-in day11: "./day11.rkt"))

(define days
  (list
    `(day1 . ,day1:day)
    `(day11 . ,day11:day)))

(~> days
    (filter (match-λ
              [(cons name _)
               (define cmdline (vector->list (current-command-line-arguments)))
               (if (< 1 (length cmdline))
                   (member (symbol->string name) (cdr cmdline))
                   #t)]) _)
    (for-each (match-λ
                [(cons name solve)
                 (displayln `(,name . ,(solve)))]) _))

