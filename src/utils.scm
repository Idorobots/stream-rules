;; Some utils.

(define-syntax ->
  (syntax-rules ()
    ((-> a (op args ...) rest ...)
     (-> (op a args ...) rest ...))
    ((-> a)
     a)))

(define-syntax ->>
  (syntax-rules ()
    ((->> a (op args ...) rest ...)
     (->> (op args ... a) rest ...))
    ((->> a)
     a)))
