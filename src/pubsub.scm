;; Publisher-subscriber thingy

(define-struct pub-sub (subscribers) #:mutable)

(define (pub-sub)
  (make-pub-sub '()))

(define (subscribe! pub-sub f)
  (set-pub-sub-subscribers! pub-sub (cons f (pub-sub-subscribers pub-sub)))
  pub-sub)

(define (publish pub-sub value)
  (map (lambda (f)
         (f value))
       (pub-sub-subscribers pub-sub))
  pub-sub)
