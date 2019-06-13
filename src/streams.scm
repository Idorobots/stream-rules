;; Streams

(load "../src/utils.scm")

(define-struct stream-stage (prev materializer))
(define-struct materialized-stream-stage (emitter))

(define (materialize stage next)
  ((stream-stage-materializer stage)
   stage
   next))

(define (push stage value)
  ((materialized-stream-stage-emitter stage) value))

(define (source)
  (make-stream-stage '()
                     (lambda (self next)
                       (make-materialized-stream-stage (curry push next)))))

(define (stage f)
  (make-stream-stage '()
                     (lambda (self next)
                       (materialize (stream-stage-prev self)
                                    (make-materialized-stream-stage (lambda (value)
                                                                      (f next value)))))))

(define (flow f)
  (stage (lambda (next value)
           (push next (f value)))))

(define (sink f)
  (stage (lambda (next value)
           (f value))))

(define (via source flow)
  (make-stream-stage source
                     (stream-stage-materializer flow)))

(define to via)

(define run-stream (curry (flip materialize) '()))

(define (run-with source sink)
  (-> source
      (to sink)
      (run-stream)))

(define (map-stream source f)
  (-> source
      (via (flow f))))

(define (filter-stream source p)
  (-> source
      (via (stage (lambda (next value)
                    (when (p value)
                      (push next value)))))))

(define (combine-streams . streams)
  (make-stream-stage '() ;; NOTE Unused.
                     (lambda (_ next)
                       (map (curry (flip materialize)
                                   (make-materialized-stream-stage (curry push next)))
                            streams))))
