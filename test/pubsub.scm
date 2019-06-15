(load "../test/testing.scm")
(load "../src/pubsub.scm")
(load "../src/utils.scm")

(describe
 "pub-sub"
 (it "should allow multiple subscribers"
     (define v1 0)
     (define v2 0)
     (define ps (pub-sub))
     (-> ps
         (subscribe! (lambda (_)
                       (set! v1 (+ 1 v1))))
         (subscribe! (lambda (_)
                       (set! v2 (+ 1 v2)))))
     (assert equal? v1 0)
     (assert equal? v2 0)
     (publish ps 23)
     (assert equal? v1 1)
     (assert equal? v2 1)
     (publish ps 5)
     (assert equal? v1 2)
     (assert equal? v2 2)))
