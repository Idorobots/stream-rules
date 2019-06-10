;; Reactive streams

(define-struct stream-stage (next emitter) #:mutable)

(define (source)
  (make-stream-stage '() push))

(define (push stage value)
  ((stream-stage-emitter stage) (stream-stage-next stage) value))

(define (flow f)
  (make-stream-stage '()
                     (lambda (next value)
                       (push next (f value)))))

(define (via source flow)
  (when (null? (stream-stage-next source))
    (set-stream-stage-next! source flow)
    flow))

(define (sink f)
  (make-stream-stage '()
                     (lambda (_ value)
                       (f value))))

(define to via)

(define (run! stream)
  stream)

(define (map-stream source f)
  (via source (flow f)))

(define (filter-stream source p)
  (via source (make-stream-stage '()
                                 (lambda (next value)
                                   (when (p value)
                                     (push next value))))))
