(define-module (day1)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 textual-ports)
  #:use-module (util)
  #:use-module (input)
  #:export (day))

(define (parse-instruction instr)
  (define rest (string-drop instr 1))
  (define n (string->number rest))
  (case (string-ref instr 0)
    ((#\L) (- n))
    ((#\R) (+ n))))

(define parse
  (->>
    (map (->> (string-trim-both _ char-whitespace?)) _)
    (filter (negate string-empty?) _)
    (map parse-instruction _)))

(define (apply-instruction instr cursor)
  (modulo (+ instr cursor) 100))

(define part1
  (->>
    (scan apply-instruction 50 _)
    (count zero? _)))

(define (part2 instrs)
  (let go ([acc 0] [cursor 50] [instrs instrs])
    (match instrs
      [() acc]
      [(0 . instrs) (go acc cursor instrs)]
      [(instr . instrs)
       (let* ([dir (sign instr)]
              [v (modulo (+ cursor dir) 100)])
         (go (if (= v 0) (+ 1 acc) acc)
             v
             (cons (- instr dir) instrs)))])))


(define-day day 1 'real
  (λ (port called-directly)
    (define instrs (parse (read-lines port)))
    (list(part1 instrs) (part2 instrs))))

