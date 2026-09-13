(define-module (day10)
  #:use-module (input)
  #:use-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:export (day))

(define (parse-schematic str)
  (-> str
      (string-filter (λ (c) (not (member c `(#\( #\))))) _)
      (string-split _ #\,)
      (curry map string->number)
      (sort _ <)))

(define-record-type machine
  (make-machine diagram wiring joltage)
  machine?
  (diagram machine-diagram)
  (wiring machine-wiring)
  (joltage machine-joltage))

(define (parse-machine str)
  (define parts (string-split str #\space))
  (define diagram (-> parts car
                      (curry string-filter (char-set #\. #\#))
                      string->list (curry map (curry char=? #\#)) list->bitvector))
  (define joltage (-> (last parts)
                      (string-trim-both _ (char-set #\{ #\}))
                      (string-split _ #\,)
                      (map string->number _)))
  (-> parts
      (drop-right _ 1)
      (drop _ 1)
      (map parse-schematic _)
      (define schematic _))
  (make-machine diagram schematic joltage))

(define (parse port)
  (-> (read-lines port)
      (map string-trim-both _)
      (filter (->> (string-prefix? ";" _) not) _)
      (filter (negate string-empty?) _)
      (map parse-machine _)))

(define (bitvector-toggle-bit! vec idx)
  (if (bitvector-bit-set? vec idx)
    (bitvector-clear-bit! vec idx)
    (bitvector-set-bit!   vec idx)))

(define (add-wiring lights wiring)
  (for-each (λ (light) (bitvector-toggle-bit! lights light)) wiring)
  lights)

(define (add-joltages . joltages)
  (apply map `(,+ . ,joltages)))


(define* (minimum less xs #:optional (default #f))
  (if (null? xs)
    default
    (let loop ([m (car xs)] [xs (cdr xs)])
      (match xs
        [() m]
        [(x . xs)
         (loop (if (less x m) x m) xs)]))))

(define (steps-length steps) (apply + (map cdr steps)))

(define (recursive-solve paths target-joltage)
  (define joltage-length (length target-joltage))
  (define (wiring->joltage wiring)
    (map (λ (i) (if (member i wiring) 1 0)) (iota joltage-length)))

  (if (= 0 (apply + target-joltage))
    0
    (and-let* ([paths-to-even (hash-ref paths (list->bitvector (map odd? target-joltage)))])
      (minimum
        <
        (filter-map
          (λ (path)
            (define delta-to-even (if (null? path)
                                    (make-list joltage-length 0)
                                    (apply add-joltages (map wiring->joltage path))))
            (define even-joltages (map - target-joltage delta-to-even))
            (and-let* ([all-non-negative (all (negate negative?) even-joltages)]
                       [sub-solution (recursive-solve paths (map (->> (/ _ 2)) even-joltages))])
              (+ (length path) (* 2 sub-solution))))
          paths-to-even)))
    ))

(define (combinations n xs)
  (if (= 0 n)
    '(())
    (pair-fold
      (λ (xs rest)
        (append
          (map (->> (cons (car xs) _)) (combinations (- n 1) (cdr xs)))
          rest))
      '()
      xs)))

(define (compute-paths-to-diagrams diagram-size wirings)
  (define wirings-length (length wirings))
  (define paths (make-hash-table))
  (define (add-path diagram steps)
    (hash-set! paths diagram (cons steps (or (hash-ref paths diagram) '()))))
  (do ((path-len 0 (+ 1 path-len))) ((> path-len wirings-length) paths)
      (for (combinations path-len wirings)
           (λ (steps)
             (define diagram (make-bitvector diagram-size))
             (for steps
                  (λ (wiring)
                    (add-wiring diagram wiring)))
             (add-path diagram steps)))))

(define-day day 10 'real
  (λ (port called-directly)
    (define machines (parse port))

    (define machines-with-paths
      (map
        (λ (machine)
          (cons machine
                (compute-paths-to-diagrams
                  (bitvector-length (machine-diagram machine))
                  (machine-wiring machine))))
        machines))

    (define part1
      (-> machines-with-paths
          (curry map
                 (->>
                   (match-lambda
                     [(machine . paths)
                      (hash-ref paths (machine-diagram machine))])
                   (curry map length)
                   (curry apply min)))
          (apply + _)))

    (define part2
      (-> machines-with-paths
          (curry map (match-lambda
                       [(machine . paths)
                        (recursive-solve paths (machine-joltage machine))]))
          (apply + _)))

    `(,part1 ,part2)))

