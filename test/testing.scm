(require racket/format)

(define-struct test-failed-exception (msg))

(define-struct assert-exception (predicate expression value expected))

(define (assert->string e)
  (format "~a did not satisfy ~a\n\texpected: ~a\n\treceived: ~a\n"
          (assert-exception-expression e)
          (assert-exception-predicate e)
          (assert-exception-expected e)
          (assert-exception-value e)))

(define-syntax describe
  (syntax-rules ()
    ((describe what body ...)
     (map (lambda (thunk)
            (with-handlers ((test-failed-exception?
                             (lambda (e)
                               (display (string-append what " - " (test-failed-exception-msg e)))
                               (newline))))
              (thunk)))
          (list body ...)))))

(define-syntax it
  (syntax-rules ()
    ((it what body ...)
     (lambda ()
       (with-handlers ((assert-exception?
                        (lambda (e)
                          (raise (make-test-failed-exception
                                  (string-append what ": " (assert->string e)))))))
         body ...)))))

(define-syntax assert
  (syntax-rules ()
    ((assert predicate value expected)
     (let ((v value)
           (e expected))
       (unless (predicate v e)
         (raise (make-assert-exception predicate 'value v e)))))
    ((assert predicate value)
     (let ((v value))
       (unless (predicate v)
         (raise (make-assert-exception predicate 'value v #t)))))))

(define true? (compose not false?))
