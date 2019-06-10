(load "../test/testing.scm")
(load "../src/streams.scm")
(load "../src/utils.scm")

(describe
 "stream"
 (it "source should allow pushing"
     (define in (source))
     (define out (sink number?))
     (-> in (to out) (run!))
     (assert false? (push in 'foo))
     (assert true? (push in 23)))
 (it "flow should map values"
     (define in (source))
     (define f (flow number?))
     (define out (sink identity))
     (-> in (via f) (to out) (run!))
     (assert false? (push in 'foo))
     (assert true? (push in 23)))
 (it "sink should not propagate events"
     (define in (source))
     (define f (flow number?))
     (define out (sink identity))
     (assert stream-stage? (-> in (to out) (via f) (run!)))
     (assert equal? (push in 'foo) 'foo)
     (assert equal? (push in 23) 23))
 (it "to should not re-connect stream stages"
     (define in (source))
     (define out (sink number?))
     (assert stream-stage? (-> in (to out) (run!)))
     (assert void? (-> in (to out) (run!))))
 (it "via should not re-connect stream satges"
     (define in (source))
     (define f (flow identity))
     (define out (sink number?))
     (assert stream-stage? (-> in (via f) (run!)))
     (assert void? (-> in (via f) (run!))))
 (it "should be composeable"
     (define in (source))
     (define v #f)
     (-> in
         (via (flow (lambda (value)
                      (set! v value))))
         (to (sink identity))
         (run!))
     (push in 23)
     (assert equal? v 23))
 (it "should be mappable"
     (define in (source))
     (define v #f)
     (-> in
         (map-stream (lambda (value)
                       (set! v value)))
         (to (sink identity))
         (run!))
     (push in 23)
     (assert equal? v 23))
  (it "should be filterable"
     (define in (source))
     (define v #f)
     (-> in
         (filter-stream number?)
         (to (sink (lambda (value)
                     (set! v value))))
         (run!))
     (push in 'foo)
     (assert equal? v #f)
     (push in 23)
     (assert equal? v 23)))
