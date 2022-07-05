;;;;;;;;;;;;;;;
;; recursive ;;
;;;;;;;;;;;;;;;

(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))

(fact 5) ; => 120

(define (list*2 ls)
  (if (null? ls)
      '()
      (cons (* 2 (car ls))
            (list*2 (cdr ls)))))

(list*2 '(1 2 3)) ; => (2 4 6)

;;;;;;;;;;;;;;;;;;;;
;; tail recursive ;;
;;;;;;;;;;;;;;;;;;;;
(define (fact-tail n)
  (fact-rec n n))

(define (fact-rec n p)
  (if (= n 1)
      p
      (let ((m (- n 1)))
	(fact-rec m (* p m)))))

(fact-tail 5) ; 120

;;;;;;;;;;;;;;;;
;; named let  ;;
;;;;;;;;;;;;;;;;
(define (fact-let n)
  (let loop((n1 n) (p n))           ; 1
    (if (= n1 1)                    
	p
	(let ((m (- n1 1)))
	  (loop m (* p m))))))      ; 2

(fact-let 5) ; 120

;;;;;;;;;;;;
;; letrec ;;
;;;;;;;;;;;;
(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
		   (if (= n1 1)
		       p
		       (let ((m (- n1 1)))
			 (iter m (* p m)))))))     ; *
    (iter n n)))

(fact-letrec 5) ; 120 

;;;;;;;;;;;;;;;;;;;
;; do expression ;;
;;;;;;;;;;;;;;;;;;;
(define (fact-do n)
  (do ((n1 n (- n1 1))
       (p n (* p (- n1 1))))
      ((= n1 1) p)))

(fact-do 5) ; 120 
