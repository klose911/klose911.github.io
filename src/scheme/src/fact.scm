;;;;;;;;;;;;;;;;;;;;;;;
;; normal recursive  ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define (fact n)
  (if (> n 1)
      (* n (fact (- n 1)))
      1))

(fact 1)
(fact 2)
(fact 10)

;;;;;;;;;;;;;;;;;;;;
;; tail recursive ;;
;;;;;;;;;;;;;;;;;;;;
(define (t-fact n)
  (letrec ((iter
	    (lambda (m p)
	      (if (= m 1)
		  p
		  (iter (- m 1) (* m p))))))
    (iter n 1)))  

(t-fact 1)
(t-fact 2)
(t-fact 10)

;;;;;;;;;;;;;;;;;;;;
;; CPS recursive  ;;
;;;;;;;;;;;;;;;;;;;;
(define (k-fact n k)
  (if (= n 0) 
      (k 1)
      (k-fact (- n 1)
              (lambda (x) (k (* n x))))))

(define (return x)
  x)

(k-fact 0 return) 
(k-fact 1 return) 
(k-fact 10 return)

;;;;;;;;;;;;;;;;;;;
;; y combinator  ;;
;;;;;;;;;;;;;;;;;;;
(define (Y F)
  (define (W P)
    (F (lambda (x) ((P P) x))))
  (W W))

(define (y-fact n)
  ((Y (lambda (arg)
        (lambda (n)
          (if (zero? n)
              1
              (* n (arg (- n 1))))))) n))

(y-fact 1)
(y-fact 2)
(y-fact 10) 


;;;;;;;;;;;;;;;;;;;;;;;;
;; map reduce version ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (r-fact n)
  (reduce * 1 (enumerate-interval  1 n)))

(r-fact 1)
(r-fact 2)
(r-fact 10) 

;;;;;;;;;;;;;;;;;;;;;;;;
;; stream map version ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define integers (cons-stream 1 (add-streams ones integers))) 

(define fact-stream
  (cons-stream 1
	       (mul-stream fact-stream integers)))   

(define (s-fact n)
  (stream-ref fact-stream n))
	       
(s-fact 1)
(s-fact 2)
(s-fact 10) 
