(load "../test/testing.scm")
(load "../src/bindings.scm")

(describe
 "logical variable"
 (it "should begin with a question mark"
     (assert false? (variable? 'foo))
     (assert false? (variable? ':?foo))
     (assert true? (variable? '?foo))))

(describe
 "binding"
 (it "should support getting & setting"
     (assert equal?
             (get-binding (bindings 'key 23) 'key)
             23))
 (it "should support assertion"
     (assert has-binding? (bindings 'key 23) 'key)
     (assert false? (has-binding? (bindings 'key 23) 'foo))
     (assert true? (empty-bindings? (bindings)))
     (assert false? (empty-bindings? (bindings 'key 23))))
 (it "should support mapping"
     (assert equal?
             (map-bindings (bindings 'key1 23 'key2 5)
                           cons)
             '((key1 . 23)
               (key2 . 5))))
 (it "should support set operations"
     (assert equal? (bindings-keys (bindings 'key1 23 'key2 5)) (set 'key1 'key2))
     (assert equal? (bindings-values (bindings 'key1 23 'key2 5)) (set '23 '5))
     (assert equal?
             (merge-bindings (bindings 'key1 23) (bindings 'key2 5))
             (bindings 'key1 23 'key2 5))
     (assert equal?
             (merge-bindings (bindings 'key1 23) (bindings 'key1 5))
             (bindings 'key1 23))))
