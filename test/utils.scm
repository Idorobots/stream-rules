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
