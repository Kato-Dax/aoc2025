(use-modules (util) (ice-9 threads) (ice-9 match))

(define days
  (list
    `(day1 ,(@ (day1) day))
    `(day2 ,(@ (day2) day))
    `(day3 ,(@ (day3) day))
    ))

(-> days
    (curry map (match-lambda
                 [(name solve) `(,name ,(call-with-new-thread solve))]))
    (curry for-each (match-lambda
                      [(name thread)
                       (define solution (join-thread thread))
                       (display `(,name (part1 ,(car solution)) (part2 ,(cadr solution))))
                       (newline)])))
