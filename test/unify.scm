(load "../test/testing.scm")
(load "../src/unify.scm")

(describe
 "consistent?"
 (it "should return true for empty intersection"
     (assert true? (consistent? '() (bindings) (bindings)))
     (assert true? (consistent? '() (bindings 'foo 23) (bindings 'bar 5))))
 (it "should correctly check consistence"
     (assert true? (consistent? '(foo) (bindings 'foo 23) (bindings 'foo 23)))
     (assert false? (consistent? '(foo) (bindings 'foo 23) (bindings 'foo 5)))))

(describe
 "unification"
 (it "should fail if passed a nil"
     (assert null? (unify '() (bindings '?key 23)))
     (assert null? (unify (bindings '?key 23) '()))
     (assert null? (unify '() '())))
 (it "should unify empty sets of bindings"
     (assert empty-bindings? (unify (bindings) (bindings))))
 (it "should unify disjoint sets of bindings"
     (assert equal?
             (unify (bindings 'foo 23)
                    (bindings 'bar 5))
             (bindings 'foo 23 'bar 5))
     (assert equal?
             (unify (bindings 'foo 23 'far 5)
                    (bindings 'bar 5 'boo 23))
             (bindings 'foo 23 'far 5 'bar 5 'boo 23)))
 (it "should unify sets of bindings correctly"
     (assert equal?
             (unify (bindings 'foo 23)
                    (bindings 'foo 5))
             '())
     (assert equal?
             (unify (bindings 'foo 23)
                    (bindings 'foo 23))
             (bindings 'foo 23))
     (assert equal?
             (unify (bindings 'foo 23 'bar 5)
                    (bindings 'foo 23 'baz 5))
             (bindings 'foo 23 'bar 5 'baz 5))
     (assert equal?
             (unify (bindings 'foo 23 'bar 5)
                    (bindings 'foo 23 'bar 5))
             (bindings 'foo 23 'bar 5))))

(describe
 "pattern matching"
 (it "should not bind if pattern doesn't match"
     (assert equal?
             (pattern-match 23 5)
             '())
     (assert equal?
             (pattern-match '(1 2 3) 5)
             '())
     (assert equal?
             (pattern-match '(1 2 3) '(1 2 5))
             '())
     (assert equal?
             (pattern-match '(1 2 (3 4 5)) '(1 2 (3 4 23)))
             '()))
 (it "should bind nothing if pattern doesn't have variables"
     (assert equal?
             (pattern-match 23 23)
             (bindings))
     (assert equal?
             (pattern-match '(1 2 3) '(1 2 3))
             (bindings))
     (assert equal?
             (pattern-match '(1 2 (3 4 5)) '(1 2 (3 4 5)))
             (bindings)))
 (it "should bind simple variables"
     (assert equal?
             (pattern-match '?foo 23)
             (bindings '?foo 23))
     (assert equal?
             (pattern-match '?foo (list 23 5))
             (bindings '?foo (list 23 5))))
 (it "should match lists"
     (assert equal?
             (pattern-match '(list 1 ?foo 3)
                            '(list 1 2 3))
             (bindings '?foo 2))
     (assert equal?
             (pattern-match '(list 1 ?foo 4)
                            '(list 1 (2 3) 4))
             (bindings '?foo '(2 3))))
 (it "should bind multiple variables correctly"
     (assert equal?
             (pattern-match '(list 1 ?foo 3 ?bar)
                            '(list 1 2 3 4))
             (bindings '?foo 2 '?bar 4))
     (assert equal?
             (pattern-match '(list 1 ?foo 3 ?bar)
                            '(list 1 2 3 (4 5 6)))
             (bindings '?foo 2 '?bar '(4 5 6)))
     (assert equal?
             (pattern-match '(list 1 ?foo 3 ?foo)
                            '(list 1 2 3 2))
             (bindings '?foo 2))
     (assert equal?
             (pattern-match '(list 1 ?foo 3 ?foo)
                            '(list 1 2 3 4))
             '()))
 (it "should be able to match partial lists"
     (assert equal?
             (pattern-match '(1 2 3 . ?rest)
                            '(1 2 3 4 5))
             (bindings '?rest '(4 5)))
     (assert equal?
             (pattern-match '(1 2 3 . ?rest)
                            '(1 2 3))
             (bindings '?rest '()))
     (assert equal?
             (pattern-match '(1 2 3 . ?rest)
                            '(1 2))
             '())))
