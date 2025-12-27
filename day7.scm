(define-module (day7)
  #:use-module (input)
  #:use-module (util)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 control)
  #:export (day))

(define (parse port)
  (-> (read-lines port)
      (curry map string-trim-both)
      (curry filter (negate string-empty?))
      (curry map string->list)
      (define lines _))
  (define height (length lines))
  (define width (length (car lines)))

  (define grid (make-array #f width height))

  (for-each
    (λ (y line)
       (for-each
         (λ (x char)
            (define cell (case char
                           [(#\.) #f]
                           [(#\S) 'start]
                           [(#\^) 'splitter]
                           [else (error 'parse `(x ,x y ,y char ,char))]))
            (array-set! grid cell x y))
         (iota width)
         line))
    (iota height)
    lines)

  grid)

(define (emit grid x y quantum)
  (define split-value (if quantum 0 1))
  (define end-value (if quantum 1 0))
  (let emit ([x x] [y y])
    (call/ec (λ (return)
      (unless (array-in-bounds? grid x y)
        (return 0))
      (when (equal? 'beam (array-ref grid x y))
        (return 0))
      (unless quantum
        (array-set! grid 'beam x y))
      (unless (array-in-bounds? grid x (+ y 1))
        (return end-value))
      (define res
        (match (array-ref grid x (+ y 1))
          ['splitter
           (+ split-value
              (emit (+ x 1) (+ y 1))
              (emit (- x 1) (+ y 1)))]
          [#f
           (emit x (+ y 1))]
          ['beam 0]
          [(? number? res) res]
          [else (error 'emit `(x ,x y ,y))]))
      (when quantum
        (array-set! grid res x y))
      res
      ))))

(define (find-start grid)
  (match-let ([(width height) (array-dimensions grid)])
    (call/ec (λ (return)
                (do ([x 0 (1+ x)]) ((>= x width))
                  (do ([y 0 (1+ y)]) ((>= y height))
                    (when (equal? 'start (array-ref grid x y))
                      (return x y))))
                (error 'find-start)))))

(define (day)
  (with-input 7
    (λ (port)
       (define input (parse port))
       (let*-values ([(x y) (find-start input)]
                     [(part1) (emit (array-copy input) x y #f)]
                     [(part2) (emit (array-copy input) x y #t)])
         `(,part1 ,part2)))))

