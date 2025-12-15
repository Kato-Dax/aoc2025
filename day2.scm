(define-module (day2)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 curried-definitions)
  #:use-module (util)
  #:use-module (input)
  #:export (day))

(define (parse-range str)
  (match (string-split str #\-)
    [(start end) (map string->number `(,start ,end))]))

(define parse
  (->>
    string-trim-both
    (string-split _ #\,)
    (map parse-range _)))

(define (valid-repititions-lengths range)
  (match range
    [(start end)
     (define len (string-length (number->string end)))
     (define start-len (string-length (number->string start)))
     (define sequence-lengths (iota (- len 1) 1))
     (filter-map
       (lambda (sequence-len)
         (define repeats (filter
                           (lambda (repeats)
                             (and
                               (<= (* sequence-len repeats) len)
                               (>= (* sequence-len repeats) start-len)))
                           (iota len 2)))
         (and (not (null? repeats)) `(,sequence-len ,repeats)))
       sequence-lengths)
     ]))

(define (split-into-sequences n sequence-len repeats)
  (define str (number->string n))
  (-> (iota repeats)
    (curry filter-map
           (λ (i)
              (string-take-at-most (string-drop-at-most str (* i sequence-len)) sequence-len)))
    (curry filter (negate string-empty))))

(define ((find-in-range max-repititions) range)
  (match-let ([(start end) range])
    (define solutions-for-repitition
      (match-lambda
        [(sequence-len repeats)
         (define len (string-length (number->string end)))
         (define (pad s)
           (if (< (string-length s) len)
             (string-pad s len #\0)
             s))
         (define (decr-sequence seq)
           (number->string (- (string->number seq) 1)))
         (define sstart (pad (number->string start)))
         (define send   (pad (number->string end)))

         (-> repeats
           (curry filter (->> (<= _ max-repititions)))
           (append-map
             (lambda (repeats)
               (define highest-sequence (car (sort
                                               (append
                                                 (split-into-sequences end sequence-len repeats)
                                                 (split-into-sequences start sequence-len repeats))
                                               string>?)))
               (let go ([seq highest-sequence])
                 (define assembled (pad (apply string-append (list-repeat seq repeats))))
                 (define next-seq (decr-sequence seq))
                 (cond
                   [(string>? assembled send)
                    (go (decr-sequence seq))]
                   [(string<? assembled sstart)
                    '()]
                   [(< (string-length next-seq) (string-length seq))
                    `(,assembled)]
                   [(cons assembled (go (decr-sequence seq)))])))
             _)
           (map string->number _))]))
    (-> range
      valid-repititions-lengths
      (append-map solutions-for-repitition _)
      delete-duplicates)))

(define (day)
  (-> (with-input 2 get-string-all) parse (define ranges _))
  (list
    (apply + (append-map (find-in-range 2) ranges))
    (apply + (append-map (find-in-range (inf)) ranges))))

