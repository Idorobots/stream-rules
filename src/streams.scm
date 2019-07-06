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
                       (materialize (lazy-stream next)
                                    next))))

(define (source)
  (make-stream-stage '()
                     (lambda (_ next)
                       (make-materialized-stream-stage (curry push next)))))

(define (source-subscribe! ps)
  (make-stream-stage '()
                     (lambda (_ next)
                       (subscribe! ps (curry push next)))))

(define (source-single value)
  (make-stream-stage '()
                     (lambda (_ next)
                       (push next value))))

(define (stage f)
  (make-stream-stage '()
                     (lambda (self next)
                       (materialize (stream-stage-prev self)
                                    (make-materialized-stream-stage (lambda (value)
                                                                      (f next value)))))))

(define (flow f)
  (stage (lambda (next value)
           (push next (f value)))))

(define (flat-flow f)
  (stage (lambda (next value)
           (materialize (f value) next))))

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

(define (flat-map-stream f stream)
  (-> stream
      (via (flat-flow f))))

(define (filter-stream p stream)
  (-> stream
      (via (stage (lambda (next value)
                    (when (p value)
                      (push next value)))))))

;;            ____ a
;; stream ___/____ b
;;           \___ ...
(define (split-stream stream . substreams)
  (delay-until-materialization
   (lambda (_)
     (let ((ps (pub-sub)))
       (map (lambda (substream)
              (-> (source-subscribe! ps)
                  (substream)
                  (run-stream)))
            substreams)
       (-> stream
           (to (sink-publish ps)))))))

;;  a ____
;;  b ____\____ next
;; ... ___/
(define (merge-streams . streams)
  (make-stream-stage '() ;; NOTE Unused.
                     (lambda (_ next)
                       (map (curry (flip materialize)
                                   (make-materialized-stream-stage (curry push next)))
                            streams))))

;;            ____ a ____
;; stream ___/____ b ____\____ next
;;           \___ ... ___/
(define (phi-stream stream . substreams)
  (delay-until-materialization
   (lambda (next)
     (let ((ps (pub-sub)))
       (map (lambda (substream)
              (materialize (substream (source-subscribe! ps))
                           (make-materialized-stream-stage (curry push next))))
            substreams)
       (-> stream
           (to (sink-publish ps)))))))
