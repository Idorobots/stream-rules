(load "../test/testing.scm")
(load "../src/streams.scm")
(load "../src/utils.scm")

(describe
 "push"
 (it "should emit values"
     (define v #f)
     (define in (make-materialized-stream-stage (lambda (value)
                                                  (set! v value))))
     (push in 23)
     (assert equal? v 23)))

(describe
 "stages"
 (it "source should allow pushing"
     (define in (-> (source)
                    (run-with (sink number?))))
     (assert false? (push in 'foo))
     (assert true? (push in 23)))
 (it "source-single should push during materialization"
     (define v #f)
     (define in (-> (source-single 23)
                    (run-with (sink (lambda (value)
                                      (set! v value))))))
     (assert equal? v 23))
 (it "flow should map values"
     (define in (-> (source)
                    (via (flow number?))
                    (run-with (sink identity))))
     (assert false? (push in 'foo))
     (assert true? (push in 23)))
 (it "sink should not propagate events"
     (define in (-> (source)
                    (to (sink identity))
                    (via (flow number?))
                    (run-stream)))
     (assert materialized-stream-stage? in)
     (assert equal? (push in 'foo) 'foo)
     (assert equal? (push in 23) 23)))

(describe
 "internals"
 (it "run should not re-connect stream stages"
     (define in (-> (source)
                    (run-with (sink number?))))
     (assert materialized-stream-stage? in)
     (assert equal? (with-handlers ((exn:fail?
                                     (constantly 'error)))
                      (run-stream in)) 'error))
 (it "delay-until-materialization should not eval lazy-stream until run"
     (define v #f)
     (define s (->> (lambda (_)
                      (begin (set! v 23)
                             (->> (source)
                                  (map-stream (lambda (value)
                                                (set! v value))))))
                    (delay-until-materialization)))
     (assert stream-stage? s)
     (assert false? v)
     (define in (-> s (run-with (sink identity))))
     (assert equal? v 23)
     (push in 5)
     (assert equal? v 5)))

(describe
 "operations"
 (it "should be composeable"
     (define v #f)
     (define in (-> (source)
                    (via (flow (lambda (value)
                                 (set! v value))))
                    (run-with (sink identity))))
     (push in 23)
     (assert equal? v 23))
 (it "should be mappable"
     (define v #f)
     (define in (-> (map-stream (lambda (value)
                                  (set! v value))
                                (source))
                    (run-with (sink identity))))
     (push in 23)
     (assert equal? v 23))
 (it "should be flat-mappable"
     (define v #f)
     (define s (->> (source-single 23)
                    (map-stream (lambda (value)
                                  (set! v value)))))
     (assert equal? v #f)
     (assert stream-stage? s)
     (define in (-> (flat-map-stream (lambda (_) s)
                                      (source))
                     (run-with (sink identity))))
     (assert equal? v #f)
     (push in 5)
     (assert equal? v 23))
 (it "should be filterable"
     (define v #f)
     (define in (-> (filter-stream number? (source))
                    (run-with (sink (lambda (value)
                                      (set! v value))))))
     (push in 'foo)
     (assert equal? v #f)
     (push in 23)
     (assert equal? v 23))
 (it "should be mergeable"
     (define v #f)
     (define ins (-> (merge-streams (source)
                                    (source))
                     (run-with (sink (lambda (value)
                                       (set! v value))))))
     (assert list? ins)
     (push (car ins) 23)
     (assert equal? v 23)
     (push (cadr ins) 5)
     (assert equal? v 5))
 (it "should be splittable"
     (define v1 #f)
     (define v2 #f)
     (define in (-> (map-stream (lambda (value)
                                  (* 2 value))
                                (source))
                    (split-stream (lambda (stream)
                                    (-> (map-stream (lambda (value)
                                                      (set! v1 value))
                                                    stream)
                                        (to (sink identity))))
                                  (lambda (stream)
                                    (-> (map-stream (lambda (value)
                                                      (set! v2 value))
                                                    stream)
                                        (to (sink identity)))))
                    (run-stream)))
     (assert false? v1)
     (assert false? v2)
     (push in 23)
     (assert equal? v1 46)
     (assert equal? v2 46))
 (it "should be phi-able (?)"
     (define v1 #f)
     (define v2 #f)
     (define v3 0)
     (define in (-> (source)
                    (phi-stream (lambda (s)
                                  (-> s
                                      (via (flow (lambda (v)
                                                   (set! v1 v)
                                                   v)))))
                                (lambda (s)
                                  (-> s
                                      (via (flow (lambda (v)
                                                   (set! v2 v)
                                                   v))))))
                    (run-with (sink (lambda (v)
                                 (set! v3 (+ v3 v)))))))
     (push in 23)
     (assert equal? v1 23)
     (assert equal? v2 23)
     (assert equal? v3 46)))

(describe
 "pubsub"
 (it "should combine nicely with streams"
     (define v1 #f)
     (define v2 #f)
     (define ps1 (pub-sub))
     (define ps2 (pub-sub))
     (subscribe! ps2 (lambda (value)
                       (set! v2 value)))
     (-> (map-stream (lambda (value)
                       (set! v1 value)
                       value)
                     (source-subscribe! ps1))
         (run-with (sink-publish ps2)))
     (assert false? v1)
     (assert false? v2)
     (publish ps1 23)
     (assert equal? v1 23)
     (assert equal? v2 23)))
