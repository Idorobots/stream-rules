(load "../test/testing.scm")
(load "../src/rete.scm")
(load "../src/bindings.scm")

(describe
 "either"
 (it "should combine streams"
     (define v #f)
     (define ins (-> (either (source)
                             (source))
                     (run-with (sink (lambda (value)
                                       (set! v value))))))
     (assert list? ins)
     (push (car ins) 23)
     (assert equal? v 23)
     (push (cadr ins) 5)
     (assert equal? v 5)))

(describe
 "matches?"
 (it "should push only when pattern matches"
     (define v #f)
     (define in (-> (source)
                    (matches? '(list ?foo))
                    (run-with (sink (lambda (value)
                                      (set! v (get-binding value '?foo)))))))
     (push in '(not-a-list 23))
     (assert equal? v #f)
     (push in '(list 23))
     (assert equal? v 23)))

(describe
 "also"
 (it "should unify both streams"
     (define v #f)
     (define ins (-> (also (source)
                           (source))
                     (run-with (sink (lambda (value)
                                       (set! v value))))))
     (assert list? ins)
     (push (car ins) (bindings '?foo 23))
     (assert equal? v #f)
     (push (cadr ins) (bindings '?foo 5))
     (assert equal? v #f)
     (push (cadr ins) (bindings '?foo 23 '?bar 5))
     (assert equal? v (bindings '?foo 23 '?bar 5)))
 (it "should keep memory of both streams"
     (define v 0)
     (define ins (-> (also (source)
                           (source))
                     (run-with (sink (lambda (value)
                                       (set! v (+ 1 v)))))))
     (push (car ins) (bindings '?foo 23))
     (push (car ins) (bindings '?foo 5))
     (assert equal? v 0)
     (push (cadr ins) (bindings))
     (assert equal? v 2))
 (it "shouldn't create the cache until materialized"
     (define v 0)
     (define ps (pub-sub))
     (define s (->> (also (matches? (source-subscribe! ps) '(foo ?foo))
                          (matches? (source-subscribe! ps) '(bar ?foo)))
                    (map-stream (lambda (value)
                                  (set! v (+ 1 v))
                                  value))))
     (define ins (-> (either s s)
                     (run-with (sink identity))))
     (publish ps '(foo 23))
     (publish ps '(bar 23))
     (publish ps '(baz 23))
     (assert equal? v 2)))

(describe
 "whenever"
 (it "should run action whenever a rule matches"
     (define v #f)
     (define in (-> (whenever (source)
                          (lambda (value)
                            (set! v value)))
                    (run-with (sink identity))))
     (push in 23)
     (assert equal? v 23)))
