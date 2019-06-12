;; Streams

(load "../src/utils.scm")

(define-struct stream-stage (prev materializer))
(define-struct materialized-stream-stage (emitter))

(define (stage f)
  (make-stream-stage '()
                     (lambda (self next)
                       (let ((p (stream-stage-prev self)))
                         ((stream-stage-materializer p)
                          p
                          (make-materialized-stream-stage (lambda (value)
                                                            (f next value))))))))

(define (push stage value)
  ((materialized-stream-stage-emitter stage) value))

(define (source)
  (make-stream-stage '()
                     (lambda (self next)
                       (make-materialized-stream-stage (lambda (value)
                                                         (push next value))))))

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
    ((stream-stage-materializer stream) stream '())))

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

(define (either . streams)
  (make-stream-stage '()
                     (lambda (_ next)
                       (let ((self (make-materialized-stream-stage (lambda (value)
                                                                     (push next value)))))
                         (map (lambda (stream)
                                ((stream-stage-materializer stream)
                                 stream
                                 self))
                              streams)))))
