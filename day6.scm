(define-module (day6)
  #:use-module (input)
  #:use-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (ice-9 textual-ports)
  #:export (day))

(define op-procedure (match-lambda ["*" *] ["+" +] [#\* *] [#\+ +]))

(define-day day 6 'real
  (λ (port called-directly)
     (define lines (read-lines port))

     (define part1
       (-> (call/ec
             (λ (done)
                (fold
                  (λ (line number-lines)
                     (when (not (string-any char-numeric? line))
                       (done (zip
                               (-> (string-split line #\space)
                                   (curry filter (negate string-empty?))
                                   (curry map op-procedure))
                               (transpose number-lines))))
                     (cons
                       (-> (string-split line #\space)
                           (curry filter (negate string-empty?))
                           (curry map string->number))
                       number-lines))
                  '()
                  lines)))
           (curry map (match-lambda [(op nums) (apply op nums)]))
           (curry apply +)))

     (define part2
       (-> lines
           (curry map string->list)
           transpose
           (curry split (λ (column) (all (->> (equal? #\space _)) column)))
           (curry map (λ (problem)
                         (define op (op-procedure (last (car problem))))
                         (define nums
                           (-> problem
                               (curry map (->>
                                            (curry filter char-numeric?)
                                            list->string
                                            string->number))))
                         (apply op nums)))
           (curry apply +)))

     `(,part1 ,part2)))

