;;;;;;;;;;;;;;;;;;
;; continuation ;;
;;;;;;;;;;;;;;;;;;

;; (/ (- x 1) 2)
;; (lambda (val) (/ val 2))

(define (f1 w)
  (let ((y (f2 w)))
    (if (integer? y) (list 'a y) 'b)))

(define (f2 x)
  (/ (- x 1) 2))

(lambda (val)
  (let ((y (/ val 2)))
    (if (integer? y) (list 'a y) 'b)))

;; (call-with-current-continuation
;;  (lambda (cc)
;;    ...))

(define frozen '())

(append '(the call/cc returned)
	(list (call-with-current-continuation
	       (lambda (cc)
		 (set! frozen cc)
		 'a)))) ; =>  (the call/cc returned a)

(lambda (val)
  (append '(the call/cc returned)
          (list val)))

(set! frozen (lambda (val)
               (append '(the call/cc returned)
                       (list val))))

(frozen 'again) ; =>  (the call/cc returned again) 

(append '(the call/cc returned)
        (list 'again)) ; =>  (the call/cc returned again)

(frozen 'thrice) ; =>  (the call/cc returned thrice) 

(append '(the call/cc returned)
        (list 'thrice)) ; =>  (the call/cc returned thrice)

(+ 1 (frozen 'safely)) ; => (the call/cc returned safely)

(define froz1 '())
(define froz2 '())

(let ((x 0))
  (call-with-current-continuation
   (lambda (cc)
     (set! froz1 cc)
     (set! froz2 cc)))
  (set! x (+ 1 x))
  x) ; => 1

(froz1 '()) ;=> 2
(froz2 '()) ;=> 3

;;;;;;;;;
;; CPS ;;
;;;;;;;;;
(define (return x)
  x)

(define (k+ a b k)
  (k (+ a b)))

(define (k* a b k)
  (k (* a b)))

(k+ 1 2
    (lambda (x)
      (k* x 3 return))) ; => 9

;;;;;;;;;;;;;;;;;;;
;; recursive CPS ;;
;;;;;;;;;;;;;;;;;;;

;;; normal factorial
(define (fact n)
  (if (= n 1) 
      1
      (* n (fact (- n 1)))))

;;; CPS factorial
(define (kfact n k)
  (if (= n 1) 
      (k 1)
      (kfact (- n 1)
             (lambda (x) (k (* n x))))))

(kfact 5 return) ; =>  120

;;; normal
(+ 3 (fact 4)) ;=> 27

;;; CPS
(kfact 4 (lambda (x) (k+ x 3 return))) ; => 27

;;; normal
(define (product ls)
  (let loop ((ls ls) (acc 1))
    (cond
     ((null? ls) acc)
     ((zero? (car ls)) 0)
     (else (loop (cdr ls) (* (car ls) acc))))))

;;; CPS with directly return
(define (kproduct ls k)
  (let ((break k))
    (let loop ((ls ls) (k k))
      (cond
       ((null? ls) (k 1))
       ((zero? (car ls)) (break 0))
       (else (loop (cdr ls) (lambda (x) (k (* (car ls) x)))))))))

;;; normal
(+ 100 (product '(2 4 7))) ; => 156 

;;; CPS
(kproduct '(2 4 7)
          (lambda (x)
            (k+ x 100 return))) ;=> 156

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPS for exception handling  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (non-number-value-error x)
  (display "Value error: ")
  (display  x)
  (display " is not number.")
  (newline)
  'error)

(define (kproduct ls k k-value-error)
  (let ((break k))
    (let loop ((ls ls) (k k))
      (cond
       ((null? ls) (k 1))
       ((not (number? (car ls))) (k-value-error (car ls)))
       ((zero? (car ls)) (break 0))
       (else (loop (cdr ls) (lambda (x) (k (* (car ls) x)))))))))

;;; valid
(kproduct '(2 4 7) 
          (lambda (x) (k+ x 100 return)) 
          non-number-value-error) ; => 156

;;; invalid
(kproduct '(2 4 7 hoge) 
          (lambda (x) (k+ x 100 return)) 
          non-number-value-error)

;; Value error: hoge is not number.
;; error
