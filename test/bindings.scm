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
      (assert false? (has-binding? (bindings 'key 23) 'foo)))
  (it "should support mapping"
      (assert equal?
              (map-bindings (bindings 'key1 23 'key2 5)
                            cons)
              '((key1 . 23)
                (key2 . 5)))))
