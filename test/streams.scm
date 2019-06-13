(load "../test/testing.scm")
(load "../src/streams.scm")
(load "../src/utils.scm")

(describe
 "stream"
 (it "source should allow pushing"
     (define in (-> (source)
                    (run-with (sink number?))))
     (assert false? (push in 'foo))
     (assert true? (push in 23)))
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
     (assert equal? (push in 23) 23))
 (it "run should not re-connect stream stages"
     (define in (-> (source)
                    (run-with (sink number?))))
     (assert materialized-stream-stage? in)
     (assert equal? (with-handlers ((exn:fail?
                                     (constantly 'error)))
                      (run-stream in)) 'error))
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
     (define in (-> (source)
                    (map-stream (lambda (value)
                                  (set! v value)))
                    (run-with (sink identity))))
     (push in 23)
     (assert equal? v 23))
 (it "should be filterable"
     (define v #f)
     (define in (-> (source)
                    (filter-stream number?)
                    (run-with (sink (lambda (value)
                                      (set! v value))))))
     (push in 'foo)
     (assert equal? v #f)
     (push in 23)
     (assert equal? v 23))
 (it "should be combineable"
     (define v #f)
     (define ins (-> (combine-streams (source)
                                      (source))
                     (run-with (sink (lambda (value)
                                       (set! v value))))))
     (assert list? ins)
     (push (car ins) 23)
     (assert equal? v 23)
     (push (cadr ins) 5)
     (assert equal? v 5)))
