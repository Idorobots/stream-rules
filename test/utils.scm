(load "../test/testing.scm")
(load "../src/utils.scm")

(describe
 "-> macro"
 (it "should handle a single value"
     (assert equal? (-> 23) 23)
     (assert equal? (-> (list 23 5)) '(23 5)))
 (it "should handle simple application"
     (assert equal? (-> 23 (list)) '(23))
     (assert equal? (-> 23 (list) (car)) 23))
 (it "should handle application with params"
     (assert equal? (-> 23 (cons 5)) (cons 23 5))
     (assert equal? (-> 23 (cons 5) (cons 13)) (cons (cons 23 5) 13))))

(describe
 "->> macro"
 (it "should handle a single value"
     (assert equal? (->> 23) 23)
     (assert equal? (->> (list 23 5)) '(23 5)))
 (it "should handle simple application"
     (assert equal? (->> 23 (list)) '(23))
     (assert equal? (->> 23 (list) (car)) 23))
 (it "should handle application with params"
     (assert equal? (->> 23 (cons 5)) (cons 5 23))
     (assert equal? (->> 23 (cons 5) (cons 13)) (cons 13 (cons 5 23)))))

(describe
 "functionals"
 (it "constantly should return the value for any args"
     (define f (constantly 23))
     (assert equal? (f 23) 23)
     (assert equal? (f f) 23)
     (assert equal? (f 1 2 3 4 5) 23))
 (it "flip should run the function with arguments swapped"
     (assert equal? ((flip cons) 1 2) (cons 2 1))))
