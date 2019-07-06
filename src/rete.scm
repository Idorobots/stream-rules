;; Stream-based Rete implementation.

(load "../src/streams.scm")
(load "../src/unify.scm")
(load "../src/ringbuffer.scm")
(load "../src/utils.scm")

(define (also-opt cache-size as bs)
  ;; NOTE This needs to be wrapped in make-stream-stage in order to properly
  ;; NOTE defer cache creation to materialization time.
  (delay-until-materialization
   (lambda (_)
     (let ((left (ring-buffer cache-size))
           (right (ring-buffer cache-size))
           (merge (lambda (left right next value)
                    (ring-buffer-store! left value)
                    (ring-buffer-foreach right (lambda (_ v)
                                                 (push next (unify value v)))))))
       (->> (merge-streams (-> as (via (stage (curry merge left right))))
                           (-> bs (via (stage (curry merge right left)))))
            (filter-stream (compose not null?)))))))

(define also (curry also-opt 100))

(define either merge-streams)

(define (matches? stream pattern)
  (->> stream
       (map-stream (curry pattern-match pattern))
       (filter-stream (compose not null?))))

(define whenever (flip map-stream))
