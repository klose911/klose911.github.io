(define (vector-add v1 v2)
  (let ((lenv1 (vector-length v1))
        (lenv2 (vector-length v2)))
    (if (= lenv1 lenv2)
        (let ((v (make-vector lenv1)))
          (let loop ((i 0))
            (if (= i lenv1)
                v
                (begin
                  (vector-set! v i (+ (vector-ref v1 i) (vector-ref v2 i)))
                  (loop (+ 1 i))))))
        (error "different dimensions."))))
