#lang racket

(provide day)

(require threading)
(require "./input.rkt")
(require "./util.rkt")

(define (parse-device-connections line)
  (match (string-split line ":")
    [(list device-name connections)
     (cons (string-trim device-name)
           (map string-trim (string-split connections " ")))]
    [_ (error 'parse-device-connections (string-append "failed to parse device: " line))]))

(define parse
  (λ~> (map string-trim _)
       (filter non-empty-string? _)
       (map parse-device-connections _)))

(define in-progress (gensym))
(define (paths-from-to cache devices from to)
  (call/ec
    (λ (return)
      (when (equal? from to)
        (return 1))
      (define key (cons from to))
      (define cached (hash-ref cache key #f))
      (when (equal? cached in-progress)
        (return 0))
      (when cached
        (return cached))

      (define from-device (or (hash-ref devices from #f) (return 0)))

      (hash-set! cache key in-progress)
      (define paths-to
        (apply + (map
          (λ (connection)
            (paths-from-to cache devices connection to))
          from-device)))
      (hash-set! cache key paths-to)
      paths-to
      )))

(define-day day 11 'real
  (λ (port)
    (define devices (~> port
        read-lines
        parse
        make-hash))
    (define cache (make-hash))

    (define part1 (paths-from-to cache devices "you" "out"))
    (define part2
      (+
        (*
          (paths-from-to cache devices "svr" "dac")
          (paths-from-to cache devices "dac" "fft")
          (paths-from-to cache devices "fft" "out"))
        (*
          (paths-from-to cache devices "svr" "fft")
          (paths-from-to cache devices "fft" "dac")
          (paths-from-to cache devices "dac" "out"))))

    `(,part1 ,part2)))
