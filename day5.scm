(define-module (day5)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (util)
  #:use-module (input)
  #:export (day))

(define (parse port)
  (define lines (read-lines port))
  (define sections (filter (negate null?) (split "" lines)))
  (unless (= 2 (length sections))
    (error 'parse-error sections))
  (define ranges-section (car sections))
  (define ids-section (cadr sections))
  (define parse-range (λ (range)
                         (match (map string-trim-both (string-split range #\-))
                           [(start end)
                            `(,(string->number start) ,(string->number end))]
                           [else (error 'parse-error range)])))
  (define trim-lines (->>
                       (curry map string-trim-both)
                       (curry filter (negate string-empty?))))
  (values
    (-> ranges-section trim-lines (curry map parse-range))
    (-> ids-section trim-lines (curry map string->number))))

(define (is-in-range num range)
  (match range
    [(start end) (and (>= num start) (<= num end))]))

(define (touching x y)
  (match-let ([(a b) x] [(c d) y])
     (not (or (< b (- c 1)) (> a (+ 1 d))))))

(define (merge a b)
  (match-let ([(start-a end-a) a] [(start-b end-b) b])
             `(,(min start-a start-b) ,(max end-a end-b))))

(define (merge-all ranges)
  (match ranges
    [() '()]
    [(range) `(,range)]
    [(a b . rest)
     (if (touching a b)
       (merge-all (cons (merge a b) rest))
       (cons a (merge-all (cons b rest))))]))

(define (count-ids ranges)
  (apply + (map (match-lambda [(a b) (- (1+ b) a)]) ranges)))

(define-day day 5 'real
  (λ (port called-directly)
     (let-values ([(ranges nums) (parse port)])
       (define (within-any-range num) (any (curry is-in-range num) ranges))
       (define part1 (count within-any-range nums))
       (define sorted (sort ranges (λ (a b) (on car < a b))))
       (define part2 (count-ids (merge-all sorted)))
       `(,part1 ,part2))))

