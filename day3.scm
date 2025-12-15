(define-module (day3)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 control)
  #:use-module (util)
  #:use-module (input)
  #:export (day))

(define parse-bank
  (->>
    string-trim-both
    string->list
    (curry map (lambda (c) (on char->integer - c #\0)))))

(define-exception-type joltage-error &error
  make-joltage-error joltage-error?
  (n joltage-error-n)
  (bank joltage-error-bank))

(define (bank-joltage n bank)
  (call/ec
    (λ (return)
      (let go ([chosen '()] [bank bank])
        (match bank
          [() (if (= n (length chosen)) (-> chosen reverse return) #f)]
          [(battery . rest-bank)
           (let walk-back ([chosen chosen])
             (match chosen
               [() #f]
               [(c . rest)
                 (walk-back rest)
                 (when (< c battery)
                   (go (cons battery rest) rest-bank))]))
           (when (> n (length chosen))
             (go (cons battery chosen) rest-bank))
           (go chosen rest-bank)]))
      (raise-exception (make-joltage-error n bank)))))

(define (joltage->number j)
  (let go ([j (reverse j)])
    (match j
      [() 0]
      [(c . cs)
       (+ c (* 10 (go cs)))])))

(define (solve port)
  (-> (read-lines port)
      (map string-trim-both _)
      (filter (negate string-empty) _)
      (map parse-bank _)
      (define bank _))
  (define (solve n)
    (-> bank
        (curry map (->> (curry bank-joltage n) joltage->number)) 
        (curry apply +)))
  (map solve '(2 12)))

(define (day) (with-input 3 solve))

