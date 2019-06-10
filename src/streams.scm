;; Streams

(load "../src/utils.scm")

(define-struct stream-stage (prev materializer))
(define-struct materialized-stream-stage (emitter))

(define (stage f)
  (make-stream-stage '()
                     (lambda (next)
                       (make-materialized-stream-stage (lambda (value)
                                                         (f next value))))))

(define (push stage value)
  ((materialized-stream-stage-emitter stage) value))

(define (source)
  (stage push))

(define (flow f)
  (stage (lambda (next value) (push next (f value)))))

(define (sink f)
  (stage (lambda (_ value) (f value))))

(define (via source flow)
  (make-stream-stage source
                     (stream-stage-materializer flow)))

(define to via)

(define (run-stream stream)
  (when (stream-stage? stream)
    (let loop ((stage stream)
               (acc '()))
      (if (null? stage)
          acc
          (loop (stream-stage-prev stage)
                ((stream-stage-materializer stage) acc))))))

(define (run-with source sink)
  (-> source
      (to sink)
      (run-stream)))

(define (map-stream source f)
  (via source (flow f)))

(define (filter-stream source p)
  (via source
       (stage (lambda (next value)
                (when (p value)
                  (push next value))))))
