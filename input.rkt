#lang racket
(require threading)

(provide define-day)

(define (with-sample body)
  (call-with-input-file "./sample.txt" body))

(define (with-input day body)
  (define path (string-append "./day" (number->string day) ".txt"))
  (define url (string-append "https://adventofcode.com/2025/day/" (number->string day) "/input"))
  (define (download-input)
    (define session (getenv "AOC_SESSION"))
    (unless session
      (error 'missing-session-cookie (with-output-to-string (λ () (display `(day ,day))))))
    (system (string-append "curl --cookie session=" session " " url " > " path)))
  (call/ec (λ (return)
             (with-handlers ([exn:fail:filesystem?
                              (λ (_)
                                (download-input)
                                (return (call-with-input-file path body)))])
               (call-with-input-file path body)))))

(define-syntax define-day
  (syntax-rules ()
    ([_ day name body]
     (define-day day name 'real body))
    ([_ day name input-kind body]
     (define day
       (let* ([with-chosen-input (λ (b) (if (equal? input-kind 'real)
                                            (with-input name b)
                                            (with-sample b)))]
              [was-called-directly
               (equal?
                (string-append "day" (number->string name) ".rkt")
                (~> 'run-file find-system-path file-name-from-path path->string))])
         (begin
           (when was-called-directly
             (display (with-chosen-input body)) (newline))
           (λ () (with-chosen-input body))))))))
