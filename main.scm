(use-modules (util) (ice-9 control) (ice-9 threads) (ice-9 exceptions) (ice-9 match))

(define days
  (list
    `(day1 ,(@ (day1) day))
    `(day2 ,(@ (day2) day))
    `(day3 ,(@ (day3) day))
    `(day4 ,(@ (day4) day))
    `(day5 ,(@ (day5) day))
    `(day6 ,(@ (day6) day))
    `(day7 ,(@ (day7) day))
    `(day8 ,(@ (day8) day))
    ))

(define-exception-type &exception-with-stack
                       &error
                       make-exception-with-stack
                       exception-with-stack?
                       (stack-trace exception-with-stack-stack))

(define (exception-printer port key args punt)
  (cond ((and (= 1 (length args))
              (exception? (car args)))
         (display "ERROR:\n" port)
         (format-exception port (car args)))
        (else
         (punt))))

(define (format-exception port exception)
  (let ((components (simple-exceptions exception)))
    (if (null? components)
        (format port "Empty exception object")
        (let loop ((i 1) (components components))
          (cond ((pair? components)
                 (format port "  ~a. " i)
                 (format-simple-exception port (car components))
                 (when (pair? (cdr components))
                   (newline port))
                 (loop (+ i 1) (cdr components))))))))

(define (format-simple-exception port exception)
  (let* ((type (struct-vtable exception))
         (name (record-type-name type))
         (fields (record-type-fields type)))
    (cond
     ((null? fields)
      (format port "~a" name))
     ((null? (cdr fields))
      (format port "~a: ~s" name (struct-ref exception 0)))
     (else
      (format port "~a:\n" name)
      (let lp ((fields fields) (i 0))
        (let ((field (car fields))
              (fields (cdr fields)))
          (format port "      ~a: ~s" field (struct-ref exception i))
          (unless (null? fields)
            (newline port)
            (lp fields (+ i 1)))))))))


(-> days
    (curry map (match-lambda
                 [(name solve)
                  `(,name
                    ,(call-with-new-thread
                       (λ ()
                          (call/ec
                            (λ (return)
                              (with-exception-handler
                                (λ (ex)
                                  (return (make-exception
                                    ex
                                    (make-exception-with-stack (make-stack #t)))))
                                solve
                                #:unwind? #f))))))]))
    (curry map (match-lambda
                 [(name thread) 
                  (define solution (join-thread thread))
                  (match solution
                    [(? exception? ex) (display `(,name error))]
                    [solution (display `(,name ,solution))])
                  (newline)
                  `(,name ,solution)]))
    (curry for-each (match-lambda
                      [(name solution)
                       (when (exception-with-stack? solution)
                         (define ex solution)
                         (display
                           (string-append "Backtrace for error in " (symbol->string name) ":\n")
                           (current-error-port))
                         (display-backtrace (exception-with-stack-stack ex) (current-error-port))

                         (define msg (with-output-to-string (λ () (print-exception (current-output-port) #f (exception-kind ex) (exception-args ex)))))
                         (display msg (current-error-port)) (newline)
                         )])))
