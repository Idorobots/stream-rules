;; Streams

(load "../src/utils.scm")
(load "../src/pubsub.scm")

(define-struct stream-stage (prev materializer))
(define-struct materialized-stream-stage (emitter))

(define (materialize stage next)
  ((stream-stage-materializer stage)
   stage
   next))

(define (push stage value)
  ((materialized-stream-stage-emitter stage) value))

(define (delay-until-materialization lazy-stream)
  (make-stream-stage '()
                     (lambda (_ next)
                       (materialize (lazy-stream)
                                    next))))

(define (source)
  (make-stream-stage '()
                     (lambda (self next)
                       (make-materialized-stream-stage (curry push next)))))

(define (source-subscribe! ps)
  (make-stream-stage '()
                     (lambda (self next)
                       (subscribe! ps (curry push next)))))

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
  (stage (lambda (_ value)
           (f value))))

(define (sink-publish ps)
  (stage (lambda (_ value)
           (publish ps value))))

(define (via source flow)
  (make-stream-stage source
                     (stream-stage-materializer flow)))

(define to via)

(define run-stream (curry (flip materialize) '()))

(define (run-with source sink)
  (-> source
      (to sink)
      (run-stream)))

(define (map-stream f stream)
  (-> stream
      (via (flow f))))

(define (filter-stream p stream)
  (-> stream
      (via (stage (lambda (next value)
                    (when (p value)
                      (push next value)))))))

(define (merge-streams . streams)
  (make-stream-stage '() ;; NOTE Unused.
                     (lambda (_ next)
                       (map (curry (flip materialize)
                                   (make-materialized-stream-stage (curry push next)))
                            streams))))
