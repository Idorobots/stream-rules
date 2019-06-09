;; Bindings and bindings accessories.
(require racket/hash)

(define (variable? v)
  (and (symbol? v)
       (string-prefix? (symbol->string v) "?")))

(define bindings hash)
(define has-binding? hash-has-key?)
(define empty-bindings? hash-empty?)

(define add-binding hash-set)
(define get-binding hash-ref)
(define map-bindings hash-map)

(define (merge-bindings as bs)
  (hash-union as bs #:combine/key (lambda (k v1 v2) v1)))

(define bindings-keys (compose list->set hash-keys))
(define bindings-values (compose list->set hash-values))
