#lang racket

(provide debug scan read-lines sign)

(define-syntax debug
  (syntax-rules ()
    ([_] #f)
    ([_ val]
     (let ([v val])
       (display "(") (display 'val) (display "=") (display v) (display ")") (newline) v))
    ([_ val vals ... last]
     (begin
       (display "(") (display 'val) (display "=") (display val)
       (begin
         (newline) (display " ") (display 'vals) (display "=") (display vals)) ...
       (let ([l last])
         (newline) (display " ") (display 'last) (display "=") (display l)
         (display ")")
         (newline)
         l)))))

(define (scan f init xs)
  (match xs
    ('() '())
    ((cons x xs) (let ((v (f x init)))
                (cons v (scan f v xs))))))

(define (read-lines port)
  (match (read-line port)
    [(? eof-object?)
     '()]
    [line
     (cons line (read-lines port))]))

(define (sign n)
  (cond
    [(> n 0) (+ 1)]
    [(< n 0) (- 1)]
    [else 0]))
