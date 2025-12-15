(use-modules (util) (ice-9 threads) (ice-9 exceptions) (ice-9 match))

(define days
  (list
    `(day1 ,(@ (day1) day))
    `(day2 ,(@ (day2) day))
    `(day3 ,(@ (day3) day))
    `(day4 ,(@ (day4) day))
    ))

(-> days
    (curry map (match-lambda
                 [(name solve)
                  `(,name
                    ,(call-with-new-thread
                       solve
                       (λ (key . args)
                          (display key) (display args) (newline)
                          (make-exception-from-throw key args))
                       ))]))
    (curry for-each (match-lambda
                      [(name thread)
                       (define solution (join-thread thread))
                       (match (join-thread thread)
                         [(? exception? ex)
                          (display `(,name (error ,(exception-kind ex) ,(exception-irritants ex))))]
                         [solution
                           (display `(,name ,solution))])
                       (newline)])))
