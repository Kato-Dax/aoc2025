(define-module (day8)
  #:use-module (input)
  #:use-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:export (day))

(define (parse-coord str)
  (map string->number (string-split str #\,)))

(define (parse port)
  (-> (read-lines port)
      (curry map string-trim-both)
      (curry filter (negate string-empty?))
      (curry map parse-coord)))

(define (coord-distance a b)
  (match-let ([(ax ay az) a] [(bx by bz) b])
             (sqrt (+ (expt (- ax bx) 2)
                      (expt (- ay by) 2)
                      (expt (- az bz) 2)))))

(define (find-circuits junctions)
  (define junction-circuits (alist->hash-table
                              (map cons junctions (iota (length junctions)))))
  (define circuits (alist->hashq-table
                     (hash-map->list (λ (junction id) `(,id ,junction)) junction-circuits)))
  (match-lambda
    [(a . b)
     (define circuit-id-of-a (hash-ref junction-circuits a))
     (define circuit-id-of-b (hash-ref junction-circuits b))
     (unless (= circuit-id-of-a circuit-id-of-b) 
       (define circuit-of-a (hashq-ref circuits circuit-id-of-a))
       (define circuit-of-b (hashq-ref circuits circuit-id-of-b))
       (for-each
         (λ (junction)
            (hash-set! junction-circuits junction circuit-id-of-a))
         circuit-of-b)
       (hash-set! circuits circuit-id-of-a (append circuit-of-a circuit-of-b))
       (hash-remove! circuits circuit-id-of-b))
     circuits]))

(define (day)
  (with-input 8
    (λ (port)
       (define junctions (parse port))
       (define connections
         (-> (pairs junctions)
             (curry filter (match-lambda [(a . b) (not (equal? a b))]))
             (sort _ (λ (a b)
                        (on (match-lambda [(a . b) (coord-distance a b)]) < a b)))))

       (-> (take-at-most connections 1000)
           (fold (let ([generator (find-circuits junctions)])
                   (λ (connection _) (generator connection))) #f _)
           (hash-map->list cons _)
           (sort _ (λ (a b) (on length > a b)))
           (take-at-most _ 3)
           (map (->> cdr length) _)
           (apply * _)
           (define part1 _))

       (define part2
         (let ([generator (find-circuits junctions)])
           (let go ([connections connections])
             (define connection (car connections))
             (define circuits (generator connection))
             (if (= 1 (hash-count (const #t) circuits))
               (* (car (car connection)) (car (cdr connection)))
               (go (cdr connections))))))

       `(,part1 ,part2))))

