(define-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:export (scan
            string-empty?
            sign
            read-lines
            string-take-at-most
            string-drop-at-most
            list-repeat
            on
            split
            split-once
            all
            transpose
            ->
            ->>
            curry))

(define (scan f init xs)
  (match xs
    (() '())
    ((x . xs) (let ((v (f x init)))
                (cons v (scan f v xs))))))

(define (string-empty? s)
  (= 0 (string-length s)))

(define (sign n)
  (cond
    [(> n 0) (+ 1)]
    [(< n 0) (- 1)]
    [else 0]))

(define (read-lines port)
  (match (get-line port)
    ((? eof-object?) '())
    (line (cons line (read-lines port)))))

(define (string-take-at-most s n)
  (string-take s (min n (string-length s))))

(define (string-drop-at-most s n)
  (string-drop s (min n (string-length s))))

(define (list-repeat v n)
  (case n
    [(0) '()]
    [else (cons v (list-repeat v (- n 1)))]))

(define (on f binop a b)
  (binop (f a) (f b)))

(define (split del ls)
  (define test (if (procedure? del) del (λ (x) (equal? x del))))
  (match ls
    [() '()]
    [ls
      (let-values ([(a b) (span (lambda (x) (not (test x))) ls)])
        (match b
          [() (match a
                [() '()]
                [a (list a)])]
          [(_ . rest)
           (cons a (if (null? rest) '(()) (split del rest)))]))]))

(define (split-once del ls)
  (match (split del ls)
    [(a b . rest) (values a b)]
    [else #f]))

(define (all pred xs)
  (not (any (negate pred) xs)))

(define-syntax ->
  (lambda (stx)
    (define (syntax->list s)
      (define l
        (let loop ([s s])
          (cond
           [(pair? s) (cons (car s) (loop (cdr s)))]
           [else s])))
      (and (list? l) l))
    (syntax-case stx ()
      ([k val] #'val)
      ([k val (f ...) rest ...]
       (let* ((fv (syntax->list #'(f ...)))
              (contains-hole
                (let go ((xs fv))
                  (match xs
                    [(x . xs) (or (equal? '_ (syntax->datum x)) (go xs))]
                    [() #f]))))
         (if contains-hole
           (let go ((acc '()) (f fv))
             (match f
               [(x . xs)
                (if (equal? '_ (syntax->datum x))
                  (go (cons #'val acc) xs)
                  (go (cons x acc) xs))]
               [()
                (cons #'k (cons (reverse acc) #'(rest ...)))]))
           #'(k ((f ...) val) rest ...))))
      ([k val f rest ...]
       #'(k (f val) rest ...)))))

(define-syntax ->>
  (syntax-rules ()
    ; ([_ f] (lambda (v) (-> v f))
    ([_ f . fs]
     (lambda (v) (-> v f . fs)))))

(define-syntax curry
  (syntax-rules ()
    ([_ f args ...]
     (lambda (v) (f args ... v)))))

(define (transpose xs)
  (match xs
    [() '()]
    [(x) (map (->> (cons _ '())) x)]
    [(x . xs)
     (-> xs
         transpose
         (λ (rest)
            (let go ([x x] [rest rest])
              (match `(,x ,rest)
                [(() ()) '()]
                [((a . x) (r . rest))
                 (cons (cons a r) (go x rest))]
                [((a . x) ())
                 (cons (cons a '()) (go x rest))]
                [(() (r . rest))
                 (cons r (go x rest))])))
         )]))

;; (begin
;;   (define (make-generator body)
;;     (define yield-tag (make-prompt-tag 'yield))
;;     (define (yield value) (abort-to-prompt yield-tag value) #f)
;;     (define next (lambda () (body yield)))
;;     (lambda (. args)
;;       (call-with-prompt yield-tag
;;         (lambda () (next))
;;         (lambda (continue value)
;;           (set! next (lambda () (apply continue args)))
;;           value))
;;       ))
;;   (define generator (make-generator (lambda (yield) (yield 3) (yield (yield 5)))))
;;   `(,(generator) ,(generator 10) ,(generator) ,(generator))
;;   )
