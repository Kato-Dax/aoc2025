(define-module (day9)
  #:use-module (input)
  #:use-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (raylib)
  #:export (day))

(define (parse-coord str)
  (map string->number (string-split str #\,)))

(define (parse port)
  (-> (read-lines port)
      (curry map string-trim-both)
      (curry filter (negate string-empty?))
      (curry map parse-coord)
      (curry map (match-lambda [(a b) (cons a b)]))))

(define (area a b)
  (match-let ([(ax . ay) a] [(bx . by) b])
             (* (+ 1 (abs (- bx ax)))
                (+ 1 (abs (- by ay))))))

(define (is-in-polygon pos corners)
  (match-let ([(x . y) pos])
    (call/ec (λ (return)
      (odd? (let go ([corners (cons (last corners) corners)])
        (match corners
          [() 0]
          [(_) 0]
          [((bx . by) (ax . ay) . rest)
           (+ (call/ec (λ (next)
                          (cond
                            [(= bx ax)
                             (when (< y (min ay by))
                               (next 0))
                             (when (>= y (max ay by))
                               (next 0))
                             (when (= x ax)
                               (return #t))
                             (when (< x ax)
                               (next 1))
                             (when (> x ax)
                               (next 0))]
                            [(= by ay)
                             (when (not (= y ay))
                               (next 0))
                             (when (< x (min ax bx))
                               (next 0))
                             (when (> x (max ax bx))
                               (next 0))
                             (return #t)])))
                             (go (cons `(,ax . ,ay) rest)))])))))))

(define (lines-intersect? a b)
  (match-let* ([((asx . asy) . (aex . aey)) a]
               [((bsx . bsy) . (bex . bey)) b]
               [alx (min asx aex)]
               [ahx (max asx aex)]
               [blx (min bsx bex)]
               [bhx (max bsx bex)]
               [aly (min asy aey)]
               [ahy (max asy aey)]
               [bly (min bsy bey)]
               [bhy (max bsy bey)]
               [a-horizontal (= asy aey)]
               [b-horizontal (= bsy bey)])
    (and
      (not (equal? a-horizontal b-horizontal))
      (if a-horizontal
        (and
          (<= alx blx)
          (>= ahx blx)
          (> aly bly)
          (< ahy bhy))
        (lines-intersect? b a)))))

(define (line-intersects-polygon? line corners)
  (let go ([corners (cons (last corners) corners)])
    (match corners
        [() #f]
        [(a) #f]
        [(a b . rest)
         (or
           (lines-intersect? line `(,a . ,b))
           (go (cons b rest)))])))

(define (rectangle-corners a b)
  (match-let ([(ax . ay) a] [(bx . by) b])
    `((,(min ax bx) . ,(min ay by))
      (,(min ax bx) . ,(max ay by))
      (,(max ax bx) . ,(min ay by))
      (,(max ax bx) . ,(max ay by)))))

(define (rectangle-edges a b)
  (match-let ([(tl bl tr br) (rectangle-corners a b)])
    `((,tl . ,tr)
      (,tr . ,br)
      (,br . ,bl)
      (,bl . ,tl))))

(define-day day 9 'real
  (λ (port called-directly)
     (define red-tiles (parse port))

     (define is-in-loop? (cached (λ (pos) (is-in-polygon pos red-tiles))))
     (define intersects-loop? (cached (λ (line) (line-intersects-polygon? line red-tiles))))

     (define tile-combinations (pairs red-tiles))

     (define part1 (-> tile-combinations
                       (curry map (match-lambda [(a . b) (area a b)]))
                       (sort _ >)
                       car))

     (define part2 (-> tile-combinations
                       (curry filter (match-lambda [(a . b)
                                                    (and (all is-in-loop? (rectangle-corners a b))
                                                         (all (λ (edge) (not (intersects-loop? edge))) (rectangle-edges a b)))]))
                       (curry map (match-lambda [(a . b) `(,(area a b) . (,a . ,b))]))
                       (sort _ (lambda (a b) (on car > a b)))
                       car))

     (when called-directly (visualization red-tiles (cdr part2)))

     `(,part1 ,(car part2))))

(define (visualization red-tiles solution)
  (init-window 600 600 (string->pointer "aoc 2025 day 9"))
  (set-window-state flag-window-resizable)

  (define min-x     (apply min (map car red-tiles)))
  (define max-x (1+ (apply max (map car red-tiles))))
  (define min-y     (apply min (map cdr red-tiles)))
  (define max-y (1+ (apply max (map cdr red-tiles))))

  (define border-width 3)

  (define window-width  (- (get-screen-width)  (* 2 border-width)))
  (define window-height (- (get-screen-height) (* 2 border-width)))
  (define scale 0)
  (define (resize)
    (set! window-width  (- (get-screen-width)  (* 2 border-width)))
    (set! window-height (- (get-screen-height) (* 2 border-width)))
    (set! scale (min (/ (- window-width  border-width) (- max-x min-x))
                     (/ (- window-height border-width) (- max-y min-y)))))
  (resize)
  (define (scale-x x)
    (* scale x))
  (define (scale-y y)
    (* scale y))
  (define (unscale-x x)
    (/ x scale))
  (define (unscale-y y)
    (/ y scale))
  (define (proj-x x)
    (+ border-width (scale-x (- x min-x))))
  (define (proj-y y)
    (+ border-width (scale-y (- y min-y))))

  (define (deproj-x x)
    (+ (unscale-x (- x border-width)) min-x))
  (define (deproj-y y)
    (+ (unscale-y (- y border-width)) min-y))

  (define n (length red-tiles))

  (do ((i 0 (+ 1 i))) ((= 1 (window-should-close)) #f)
    (begin-drawing)

    (let ([x (floor (deproj-x (get-mouse-x)))] [y (floor (deproj-y (get-mouse-y)))])
      (if (is-in-polygon `(,x . ,y) red-tiles)
        (clear-background #xFF000080)
        (clear-background #xFF181818))
      (draw-rectangle
        (round (proj-x x))
        (round (proj-y y))
        (ceiling (scale-x 1))
        (ceiling (scale-y 1))
        #xFF00FF00))

    (unless (= 0 (is-window-resized))
      (resize)
      (set! window-width (get-screen-width))
      (set! window-height (get-screen-height)))

    (draw-rectangle-lines-ex
      (bytevector-ieee-double-native-ref (pointer->bytevector (make-c-struct (list float float) (list 0 0)) (sizeof double)) 0)
      (bytevector-ieee-double-native-ref (pointer->bytevector (make-c-struct (list float float) (list window-width window-height)) (sizeof double)) 0)
      border-width #xFF0000FF)

    (when (or (is-key-pressed key-d) (is-key-pressed-repeat key-d)) (set! n (+ n 1)))
    (when (or (is-key-pressed key-a) (is-key-pressed-repeat key-a)) (set! n (- n 1)))
    (set! n (modulo n (+ 1 (length red-tiles))))

    (map
      (λ (a b)
         (match-let* ([(ax . ay) a]
                      [(bx . by) b]
                      [(x . y) (cons (min ax bx) (min ay by))]
                      [w (scale-x (+ 1 (abs (- bx ax))))]
                      [h (scale-y (+ 1 (abs (- by ay))))])
           (draw-rectangle (round (proj-x x)) (round (proj-y y)) (ceiling w) (ceiling h) #xFF0000FF)))
      (take red-tiles n)
      (cons (last red-tiles) red-tiles))

    (-> red-tiles
        (curry for-each (match-lambda
                          [(x . y)
                           (draw-rectangle (round (proj-x x)) (round (proj-y y)) (ceiling (scale-x 1)) (ceiling (scale-y 1)) #xFFFFFFFF)])))

    (let ([a (car solution)] [b (cdr solution)])
      (for-each
        (match-lambda
          [(x . y) (draw-rectangle
                     (round (proj-x x))
                     (round (proj-y y))
                     (ceiling (scale-x 1))
                     (ceiling (scale-y 1))
                     #xFFFF0000)])
        (rectangle-corners a b))
      (draw-rectangle
        (round (proj-x (min (car a) (car b))))
        (round (proj-y (min (cdr a) (cdr b))))
        (round (scale-x (1+ (- (max (car a) (car b)) (min (car a) (car b))))))
        (round (scale-y (1+ (- (max (cdr a) (cdr b)) (min (cdr a) (cdr b))))))
        #x80FF00FF))

    (end-drawing))
  (close-window))

