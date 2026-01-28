(define-module (day4)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 control)
  #:use-module (ice-9 textual-ports)
  #:use-module (util)
  #:use-module (input)
  #:export (day))

(define (parse-grid port)
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
            (array-set! grid (char=? #\@ char) x y))
         (iota width)
         line))
    (iota height)
    lines)

  grid)

(define (neighbours x y)
  `((,(- x 1) ,(- y 1))
    (,(- x 0) ,(- y 1))
    (,(+ x 1) ,(- y 1))
    (,(- x 1) ,(- y 0))
    (,(+ x 1) ,(- y 0))
    (,(- x 1) ,(+ y 1))
    (,(- x 0) ,(+ y 1))
    (,(+ x 1) ,(+ y 1))))

(define (is-accessible grid x y)
  (> 4 (count
         (match-lambda
           [(x y) (and (array-in-bounds? grid x y) (array-ref grid x y))])
         (neighbours x y))))

(define (count-accessible grid)
  (define result 0)
  (match-let ([(width height) (array-dimensions grid)])
             (do ((x 0 (1+ x))) ((>= x width))
               (do ((y 0 (1+ y))) ((>= y height))
                 (when
                   (and
                     (array-ref grid x y)
                     (is-accessible grid x y))
                   (set! result (+ 1 result))))))
  result)

(define (remove-rolls grid)
  (define removed 0)
  (match-let ([(width height) (array-dimensions grid)])
    (do ((x 0 (1+ x))) ((>= x width) removed)
      (do ((y 0 (1+ y))) ((>= y height))
        (when (and (array-ref grid x y) (is-accessible grid x y))
          (array-set! grid #f x y)
          (set! removed (+ 1 removed)))))))

(define-day day 4 'real
  (λ (input called-directly)
     (define grid (parse-grid input))

     (define part1 (count-accessible grid))

     (define part2
       (let remove ([removed 0])
         (define removed-now (remove-rolls grid))
         (if (= 0 removed-now)
           removed
           (remove (+ removed removed-now)))))

     `(,part1 ,part2)))

