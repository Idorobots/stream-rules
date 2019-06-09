;; Unification & pattern matching.

(load "../src/bindings.scm")

(define (consistent? intersection as bs)
  (foldl (lambda (k r)
           (and r
                (equal? (get-binding as k)
                        (get-binding bs k))))
         #t
         intersection))

(define (unify as bs)
  (cond ((or (null? as) (null? bs))
         '())
        ((consistent? (set->list (set-intersect (bindings-keys as) (bindings-keys bs))) as bs)
         (merge-bindings as bs))
        ('else '())))

(define (pattern-match pattern value)
  (cond ((variable? pattern)
         (bindings pattern value))
        ((and (list? pattern)
              (list? value)
              (equal? (length pattern)
                      (length value)))
         (foldl unify
                (bindings)
                (map pattern-match pattern value)))
        ((equal? pattern value)
         (bindings))
        ('else '())))
