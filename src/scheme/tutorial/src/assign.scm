;;;;;;;;;;;
;; set!  ;;
;;;;;;;;;;;

(define var 1)
(set! var (* var 10))
var ;  10

(let ((i 1))
  (set! i (+ i 3))
  i) ; 4 

;;;;;;;;;;;;;;;;;;;;;
;; Lexical Closure ;;
;;;;;;;;;;;;;;;;;;;;;
(define bank-account
  (let ((balance 10))
    (lambda (n)
      (set! balance (+ balance n))
      balance)))

(bank-account 20)     ; donating 20 dollars 
;; =>  30

(bank-account -25)     ; withdrawing 25 dollars
;; => 5

(define (make-bank-account balance)
  (lambda (n)
    (set! balance (+ balance n))
    balance))

(define gates-bank-account (make-bank-account 10))   ; Gates makes a bank account by donating  10 dollars
(gates-bank-account 50)                              ; donating 50 dollars
;; 60
(gates-bank-account -55)                             ; withdrawing 55 dollars
;; 5


(define torvalds-bank-account (make-bank-account 100))  ; Torvalds makes a bank account by donating 100 dollars
(torvalds-bank-account -70)                             ; withdrawing 70 dollars
;; 30
(torvalds-bank-account 300)                             ; donating 300 dollars
;; 330

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destructive Operations on Lists ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tree '((1 2) (3 4 5) (6 7 8 9))) 
(set-car! (car tree) 100)  ; changing 1 to 100 
tree ; => ((100 2) (3 4 5) (6 7 8 9))

(set-cdr! (third tree) '(a b c)) ; changing  '(7 8 9) to '(a b c) 
tree ; =>  ((100 2) (3 4 5) (6 a b c))

;;;;;;;;;;;
;; Queue ;;
;;;;;;;;;;;
(define (make-queue)
  (cons '() '()))

(define (enqueue! queue obj)
  (let ((lobj (cons obj '())))
    (if (null? (car queue))
	(begin
	  (set-car! queue lobj)
	  (set-cdr! queue lobj))
	(begin
	  (set-cdr! (cdr queue) lobj)
	  (set-cdr! queue lobj)))
    (car queue)))

(define (dequeue! queue)
  (let ((obj (car (car queue))))
    (set-car! queue (cdr (car queue)))
    obj))

(define q (make-queue))
q ;  (()) 

(enqueue! q 'a) ;  (a)
(enqueue! q 'b) ;  (a b)
(enqueue! q 'c) ; (a b c)

(dequeue! q) ; a

q ; ((b c) c)
