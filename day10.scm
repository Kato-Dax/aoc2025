(define-module (day10)
  #:use-module (input)
  #:use-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (queue)
  #:export (day))

(define (parse-schematic str)
  (-> str
      (string-filter (λ (c) (not (member c `(#\( #\))))) _)
      (string-split _ #\,)
      (curry map string->number)))

(define-record-type machine
  (make-machine diagram wiring joltage)
  machine?
  (diagram machine-diagram)
  (wiring machine-wiring)
  (joltage machine-joltage))

(define (parse-machine str)
  (define parts (string-split str #\space))
  (define diagram (-> parts car
                      (curry string-filter (->> (member _ '(#\. #\#))))
                      string->list (curry map (->> (char=? #\# _))) list->bitvector))
  (define joltage (last parts))
  (-> parts
      (drop-right _ 1)
      (drop _ 1)
      (map parse-schematic _)
      (define schematic _))
  (make-machine diagram schematic joltage))

(define (parse port)
  (-> (read-lines port)
      (curry map string-trim-both)
      (curry filter (negate string-empty?))
      (curry map parse-machine)))

(define (bitvector-toggle-bit! vec idx)
  (if (bitvector-bit-set? vec idx)
    (bitvector-clear-bit! vec idx)
    (bitvector-set-bit!   vec idx)))

(define (add-wiring lights wiring)
  (for-each (λ (light) (bitvector-toggle-bit! lights light)) wiring)
  lights)

(define (wiring<? a b)
  (match `(,a . ,b)
    [(() . ()) #f]
    [(() . _)  #t]
    [((_ . _) ()) #t]
    [((ax . a) (bx . b))
     (cond
          [(< ax bx) #t]
          [(> ax bx) #f]
          [else (wiring<? a b)])]))

(define (solve-machine machine)
  (define shortest-paths (make-hash-table))
  (define diagram-size (bitvector-length (machine-diagram machine)))
  (define queue-init (-> machine
                         machine-wiring
                         (curry map (->> (add-wiring (make-bitvector diagram-size) _) (cons 1 _)))
                         list->queue))
  (let go ([queue queue-init])
    (match (queue-pop queue)
      [#f (error 'solve-machine)]
      [((steps . pos) . queue)
       (let* ([shortest-path (hash-ref shortest-paths pos #f)])
         (cond
           [(equal? (machine-diagram machine) pos) steps]
           [shortest-path (go queue)]
           [else
             (hash-set! shortest-paths pos steps)
             (go (fold queue-push queue
                       (map (λ (w) `(,(1+ steps) . ,(add-wiring (bitvector-copy pos) w))) (machine-wiring machine))))]))])))

(define-day day 10 'real
  (λ (port called-directly)
     (define machines (parse port))

     (define part1 (-> machines (curry map solve-machine) (apply + _)))
     (define part2 'todo)

     `(,part1 ,part2)))

