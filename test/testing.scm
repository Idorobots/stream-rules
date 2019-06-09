(require racket/exn)

(define-struct test-failed-exception (msg))

(define-syntax describe
  (syntax-rules ()
    ((describe what body ...)
     (map (lambda (thunk)
            (with-handlers ((test-failed-exception?
                             (lambda (e)
                               (display (test-failed-exception-msg e))
                               (newline))))
              (thunk)))
          (list body ...)))))

(define-syntax it
  (syntax-rules ()
    ((it what body ...)
     (lambda ()
       (with-handlers ((exn:fail?
                        (lambda (e)
                          (raise (make-test-failed-exception
                                  (string-append "'" what "' failed: " (exn->string e)))))))
         body ...)))))

(define (assert predicate value expected)
  (unless (predicate value expected)
      (error "" value " did not " predicate expected)))

(define assert-equal? (curry assert equal?))
