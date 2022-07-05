;;;;;;;;;;;;;;;;;;
;; if statement ;;
;;;;;;;;;;;;;;;;;;

(null? '()) ; #t 

(null? '(a b c)) ; #f 

(define (sum-gp a0 r n)
  (* a0
     (if (= r 1)
         n
         (/ (- 1 (expt r n)) (- 1 r)))))   ; !!


;;;;;;;;;;;;;;;;;;;;;
;; compound logic  ;;
;;;;;;;;;;;;;;;;;;;;;
;;; and
(and #f 0) ; #f
(and 1 2 3) ; #3 
(and 1 2 3 #f) ; #f 

;;; or
(or #f 0) ;  0 
(or 1 2 3) ; 1
(or #f 1 2 3) ; 1 
(or #f #f #f) ; #f

;;;;;;;;;;;;;;;;;;;;;
;; cond expression ;;
;;;;;;;;;;;;;;;;;;;;;
(define (fee age)
  (cond
   ((or (<= age 3) (>= age 65)) 0)
   ((<= 4 age 6) 0.5)
   ((<= 7 age 12) 1.0)
   ((<= 13 age 15) 1.5)
   ((<= 16 age 18) 1.8)
   (else 2.0)))

;;;;;;;;;;;;;;;;;;;;;;;
;; compare functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; eq? 
(define str "hello")

(eq? str str) ; #t 
(eq? "hello" "hello") ; #f

;;; comparing numbers depends on implementations
(eq? 1 1) ; #t 
(eq? 1.0 1.0) ; #f 

;;; eqv? 
(eqv? 1.0 1.0) ; #t 
(eqv? 1 1.0) ; #f 

;;; don't use it to compare sequences
(eqv? (list 1 2 3) (list 1 2 3)) ; #f 

(eqv? "hello" "hello") ; #f 

;;; the following depends on implementations
(eqv? (lambda(x) x) (lambda (x) x)) ; #f 

;;; equal?
(equal? (list 1 2 3) (list 1 2 3)) ; #t 
(equal? "hello" "hello") ; #t 

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type check functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(pair? '()) ; #f 
(list? '())  ; #t

;;;;;;;;;;;;;;;;;;;;
;; number compare ;;
;;;;;;;;;;;;;;;;;;;;

(= 1 1 1.0) ; #t
(< 1 2 3) ; #t 
(< 1) ; #t
(<) ; #t 

(= 2 2 2) ; #t 
(< 2 3 3.1) ; #t 
(> 4 1 -0.2) ;#t 

(<= 1 1 1.1) ;#t 

(>= 2 1 1.0) ; #t 

(<= 3 4 3.9) ; #f 
