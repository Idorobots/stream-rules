;; Stream-based Rete implementation.

(load "../src/streams.scm")
(load "../src/unify.scm")
(load "../src/ringbuffer.scm")
(load "../src/utils.scm")

(define ((zip-with-memory size) as bs f)
  (let ((left (ring-buffer size))
        (right (ring-buffer size))
        (combine (lambda (left right next value)
                   (ring-buffer-store! left value)
                   (ring-buffer-foreach right (lambda (_ v) (f next value v))))))
    (combine-streams (-> as (via (stage (curry combine left right))))
                     (-> bs (via (stage (curry combine right left)))))))

(define (also as bs)
  ((zip-with-memory 100)
   as
   bs
   (lambda (next left right)
     (let ((r (unify left right)))
       (unless (null? r)
         (push next r))))))

(define either combine-streams)

(define (matches? stream pattern)
  (-> stream
      (via (stage (lambda (next value)
                    (let ((r (pattern-match pattern value)))
                      (unless (null? r)
                        (push next r))))))))

(define whenever map-stream)
