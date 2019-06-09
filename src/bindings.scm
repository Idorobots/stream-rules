;; Bindings and bindings accessories.

(define (variable? v)
  (and (symbol? v)
       (string-prefix? (symbol->string v) "?")))

(define bindings hash)
(define add-binding hash-set)
(define has-binding? hash-has-key?)
(define get-binding hash-ref)
(define map-bindings hash-map)
