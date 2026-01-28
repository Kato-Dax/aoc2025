(define-module (input)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 exceptions)
  #:export (with-input with-sample define-day))

(define (with-sample body)
  (with-input-from-file "./sample.txt" (λ () (body (current-input-port)))))

(define (with-input day body)
  (define path (string-append "./day" (number->string day) ".txt"))
  (define (load-input) (call-with-port (open-input-file path) get-string-all))
  (define url (string-append "https://adventofcode.com/2025/day/" (number->string day) "/input"))
  (define (download-input)
    (define session (getenv "AOC_SESSION"))
    (unless session
      (error 'missing-session-cookie `(day ,day)))
    (system (string-append "curl --cookie session=" session " " url " > " path)))
  (call/ec (λ (return)
    (with-exception-handler
      (λ (exception)
         (unless ((exception-predicate &external-error) exception)
           (raise-exception exception))
         (download-input)
         (return (call-with-port (open-input-file path) body)))
      (λ () (call-with-port (open-input-file path) body))
      #:unwind? #t))))

(define-syntax define-day
  (syntax-rules ()
    ([_ day name body]
     (define-day day name 'real body))
    ([_ day name input-kind body]
     (define day
       (let* ([with-chosen-input (λ (b) (if (equal? input-kind 'real)
                                            (with-input name b)
                                            (with-sample b)))]
              [was-called-directly (or
                                     (equal? (command-line) (list (string-append "./day" (number->string name) ".scm")))
                                     (equal? (command-line) `("guile")))]
              [b (λ (port) (body port was-called-directly))])
         (begin
           (when was-called-directly
             (display (with-chosen-input b)) (newline))
           (λ () (with-chosen-input b))))))))

