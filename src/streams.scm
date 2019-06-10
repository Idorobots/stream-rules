;; Reactive streams

(define-struct stream-stage (prev materializer))
(define-struct materialized-stream-stage (emitter))

(define ((materialize f) next)
  (make-materialized-stream-stage (f next)))

(define (stage materializer)
  (make-stream-stage '() materializer))

(define (push stage value)
  ((materialized-stream-stage-emitter stage) value))

(define (source)
  (stage (materialize (lambda (next) (curry push next)))))

(define (flow f)
  (stage (materialize (lambda (next) (compose (curry push next) f)))))

(define (sink f)
  (stage (materialize (lambda (_) f))))

(define (via source flow)
  (make-stream-stage source
                     (stream-stage-materializer flow)))

(define to via)

(define (run sink)
  (when (stream-stage? sink)
    (define (run-acc stage acc)
      (if (null? stage)
          acc
          (run-acc (stream-stage-prev stage)
                   ((stream-stage-materializer stage) acc))))
    (run-acc sink '())))

(define (map-stream source f)
  (via source (flow f)))

(define (filter-stream source p)
  (via source
       (stage (materialize (lambda (next)
                             (lambda (value)
                               (when (p value)
                                 (push next value))))))))
