((lambda (x)
   (+ x 1))
 6) ;; => 7

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(fact 5) ; => 120

(define op-maker
  (lambda (op)
    (lambda (x)
      (op x))))

(define fact-maker
  (lambda (procedure)
    (lambda (n)
      (if (zero? n)
          1
          (* (procedure (- n 1)) n)))))

(define fact-maker
  (lambda (procedure)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((procedure procedure) (- n 1)))))))

((fact-maker fact-maker) 5)

(define fact 
  ((lambda (procedure)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (- n 1)))))) ; fact-maker
   (lambda (procedure)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (- n 1)))))) ; fact-maker
   ))

(fact 5) ; => 120

(
 ((lambda (procedure)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((procedure procedure) (- n 1)))))) ; fact-maker
  (lambda (procedure)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((procedure procedure) (- n 1)))))) ; fact-maker
  ) 5) ; => 120

(define F
  (lambda (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (- n 1))))))

(define F
  (lambda (n)
    (if (zero? n)
        1
        (* n ((lambda (arg) ((procedure procedure) arg)) (- n 1))))))

(define F
  ((lambda (func-arg)
     (lambda (n)
       (if (zero? n)
           1
           (* n (func-arg (- n 1))))))
   (lambda (arg) ((procedure procedure) arg))))

(define fact
  ((lambda (procedure)
     ((lambda (func-arg)
       (lambda (n)
          (if (zero? n)
              1
              (* n (func-arg (- n 1))))))
      (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     ((lambda (func-arg)
       (lambda (n)
          (if (zero? n)
              1
              (* n (func-arg (- n 1))))))
      (lambda (arg) ((procedure procedure) arg))))))

(fact 5) ; => 120

(lambda (func-arg)
  (lambda (n)
    (if (zero? n)
        1
        (* n (func-arg (- n 1))))))

(define F*
  (lambda (func-arg)
         (lambda (n)
            (if (zero? n)
                1
                (* n (func-arg (- n 1)))))))

(define fact
    ((lambda (procedure)
       (F* (lambda (arg) ((procedure procedure) arg))))
     (lambda (procedure)
       (F* (lambda (arg) ((procedure procedure) arg))))))

(define Y
  (lambda (F) 
    ((lambda (procedure)
       (F (lambda (arg) ((procedure procedure) arg))))
     (lambda (procedure)
       (F (lambda (arg) ((procedure procedure) arg)))))))

((Y F*) 5) ; => 120
((F* (Y F*)) 5) ; => 120

(define Y
  (lambda (F) 
    (let ((W (lambda (procedure)
               (F (lambda (arg) ((procedure procedure) arg))))))
      (W W))))

((Y F*) 5) ; =>120
((F* (Y F*)) 5) ; =>120

(define (Y F)
  (define (W P)
    (F (lambda (x) ((P P) x))))
  (W W)) 
    
