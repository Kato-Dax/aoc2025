(use-modules (srfi srfi-1) (ice-9 match) (util))

(define (parse-instruction instr)
  (define rest (string-drop instr 1))
  (define n (string->number rest))
  (case (string-ref instr 0)
    ((#\L) (- n))
    ((#\R) (+ n))))

(define parse
  (->>
    (map (->> (string-trim-both _ char-whitespace?)) _)
    (filter (negate string-empty) _)
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

(unless (equal? (command-line) '("guile"))
  (with-input-from-file "./day1.txt"
    (lambda ()
      (define instrs (parse (read-lines (current-input-port))))
      (display (part1 instrs)) (newline)
      (display (part2 instrs)) (newline))))

