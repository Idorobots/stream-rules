;; Ring buffer

(define-struct ring-buffer (data start end) #:mutable)

(define (ring-buffer size)
  (make-ring-buffer (make-vector size '()) 0 0))

(define (ring-buffer-ref buffer index)
  (let ((b (ring-buffer-data buffer)))
    (vector-ref b (modulo index (vector-length b)))))

(define (ring-buffer-set! buffer index object)
  (let ((b (ring-buffer-data buffer)))
    (vector-set! b (modulo index (vector-length b)) object)))

(define (next-index i buffer)
  (modulo (+ i 1) (vector-length (ring-buffer-data buffer))))

(define (ring-buffer-store! buffer object)
  (let* ((e (ring-buffer-end buffer))
         (i (next-index e buffer)))
    (ring-buffer-set! buffer e object)
    (set-ring-buffer-end! buffer i)
    (when (= i (ring-buffer-start buffer))
      ;; NOTE This makes it always store 1 less item than the capacity permits.
      (set-ring-buffer-start! buffer (next-index i buffer)))))

(define (ring-buffer-foreach buffer f)
  (let ((e (ring-buffer-end buffer)))
    (let loop ((i (ring-buffer-start buffer)))
      (when (not (= i e))
        (f i (ring-buffer-ref buffer i))
        (loop (next-index i buffer))))))
