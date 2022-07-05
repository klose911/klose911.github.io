;;;;;;;;;;;;;;;;;;;;
;; church number  ;;
;;;;;;;;;;;;;;;;;;;;
;; 丘奇数是一个单参数 f 的过程
;; f 是一个 单参数的过程
;; 返回值也是一个单参数的过程

(define ZERO
  (lambda (f) ;; zero 的参数 f 
    (lambda (x) ;; zero 的返回值
      x)))

(define (ZERO f)
  (lambda (x) x))  

;;; zero 实际上返回的是一个恒等过程

(define (inc1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

;; ((ZERO inc1) 10) ; 10 
;; ((ZERO sub1) 10) ; 10 

(define (ONE f)
  (lambda (x)
    (f x))) 

;; ((ONE inc1) 10) ; 11
;; ((ONE sub1) 10) ; 9

(define (TWO f)
  (lambda (x)
    (f (f x))))

;; ((TWO inc1) 10) ; 12
;; ((TWO sub1) 10) ; 8

(define (THREE f)
  (lambda (x)
    (f (f (f x))))) 

;; ((THREE inc1) 10) ; 13
;; ((THREE sub1) 10) ; 7

;;;;;;;;;;;;;;
;; 后继函数 ;;
;;;;;;;;;;;;;;
(define (SUCC n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;; (((SUCC ZERO) inc1) 10) ; 11
;; (((SUCC ZERO) sub1) 10) ; 9

;; (((SUCC ONE) inc1) 10) ; 12
;; (((SUCC ONE) sub1) 10) ; 8

;; (((SUCC TWO) inc1) 10) ; 13
;; (((SUCC TWO) sub1) 10) ; 7

;;;;;;;;;;;;;;
;; 加法函数 ;;
;;;;;;;;;;;;;;
(define (PLUS m n)
  (lambda (f)
    (lambda (x)
      ((m f)
       ((n f) x)))))  

;; (((PLUS ONE TWO) inc1) 10) ; 13 
;; (((PLUS ONE TWO) sub1) 10) ; 7 

(define (PLUS m n)
  ((m SUCC) n))

;; (((PLUS ONE TWO) inc1) 10) ; 13 
;; (((PLUS ONE TWO) sub1) 10) ; 7 

;;;;;;;;;;;;;;
;; 乘法函数 ;;
;;;;;;;;;;;;;;
(define (MULT m n)
  (lambda (f)
    (lambda (x) 
      ((m (n f)) x))))

;; (((MULT TWO THREE) inc1) 10 ) ; 16 
;; (((MULT TWO THREE) sub1) 10 ) ; 4  

;;;;;;;;;;
;; 逻辑运算 ;;
;;;;;;;;;;
(define TRUE
  (lambda (x y) x))
(TRUE 1 0) ; 1

(define FALSE
  (lambda (x y) y))
(FALSE 1 0) ; 0 

(define NOT
  (lambda (p)
    (p FALSE TRUE)))

;; ((NOT TRUE) 1 0) ; 0
;; ((NOT FALSE) 1 0) ; 1

(define AND
  (lambda (p q)
    (p q p)))

;; ((AND TRUE TRUE) 1 0) ; 1 
;; ((AND TRUE FALSE) 1 0) ; 0 
;; ((AND FALSE TRUE) 1 0) ; 0
;; ((AND FALSE FALSE) 1 0) ; 0 

(define OR
  (lambda (p q)
    (p p q))) 

;; ((OR TRUE TRUE) 1 0) ; 1 
;; ((OR TRUE FALSE) 1 0) ; 1
;; ((OR FALSE TRUE) 1 0) ; 1
;; ((OR FALSE FALSE) 1 0) ; 0 

(define IF-THEN-ELSE
  (lambda (p a b) 
    (p a b)))

;; (IF-THEN-ELSE TRUE 1 0) ; 1
;; (IF-THEN-ELSE FALSE 1 0) ; 0 


;;;;;;;;;;;;;;
;; 谓词函数 ;;
;;;;;;;;;;;;;;
(define (ALWAYS-FALSE x)
  FALSE) 

(ALWAYS-FALSE ZERO) ;  #<procedure:FALSE>

(define (IS-ZERO n)
  ((n ALWAYS-FALSE) TRUE))   

;; (IS-ZERO ZERO) ; #<procedure:TRUE>
;; (IS-ZERO ONE) ;  #<procedure:FALSE>

;;;;;;;;;;
;; 序对函数;;
;;;;;;;;;;
(define (CONS x y)
  (lambda (f)
    (f x y)))

;; (CONS ONE TWO) ; #<procedure> 

(define (CAR p)
  (p TRUE))

;; (CAR (CONS ONE TWO)) ; #<procedure:ONE>

(define (CDR p)
  (p FALSE))

;; (CDR (CONS ONE TWO)) ; #<procedure:TWO>

(define NIL
  (lambda (x) TRUE))

;; NIL ; #<procedure:NIL

;; 只有 p 为 NIL 的时候才会返回 TRUE 过程
(define (NULL? p) 
  (p (lambda (x y) FALSE))) 

;; (NULL? NIL) ; #<procedure:TRUE>

;; (NULL? (CONS ONE TWO)) ; #<procedure:FALSE> 
;; ((CONS ONE TWO) (lambda (x y) FALSE))
;; ((lambda (f) (f ONE TWO)) (lambda (x y) FALSE))
;; ((lambda (x y) FALSE) ONE TWO)

;;;;;;;;;;;;;;
;; 前驱函数 ;;
;;;;;;;;;;;;;;
(define fai
  (lambda (x)
    (CONS (CDR x)
	  (SUCC (CDR x)))))

;; (define p (fai (CONS ZERO ONE)))
;; p ;;; 等价于 (CONS ONE TWO) 

;; (CAR p) ; 等价于 #<procedure:ONE>
;; (((CAR p) inc1 ) 10) ; 11
;; (((CAR p) sub1 ) 10) ; 9 

;; (CDR p) ;; 等价于  #<procedure:TWO>
;; (((CDR p) inc1 ) 10) ; 12
;; (((CDR p) sub1 ) 10) ; 8 

(define PRED
  (lambda (n)
    (CAR ((n fai)
	  (CONS ZERO ZERO)))))

;; (PRED ZERO) ; #<procedure:ZERO>
;; (PRED ONE) ; #<procedure:ZERO>

;; (((PRED THREE) inc1 ) 10) ; 12
;; (((PRED THREE) sub1 ) 10) ; 8

;;;;;;;;;;;;;;
;; 减法函数 ;;
;;;;;;;;;;;;;;
(define (SUB m n)
  ((n PRED) m))

;; (((SUB THREE ONE) inc1) 10) ; 12
;; (((SUB THREE ONE) sub1) 10) ; 8

;;;;;;;;;;;;;;
;; 比较函数 ;;
;;;;;;;;;;;;;;
(define (LEQ m n)
  (IS-ZERO (SUB m n)))

;; (LEQ ONE TWO) ; #<procedure:TRUE>
;; (LEQ TWO ONE) ; #<procedure:FALSE>
;; (LEQ TWO TWO) ;  #<procedure:TRUE>

(define (EQ m n)
  (AND (LEQ m n) (LEQ n m)))

;; (EQ ONE TWO) ; #<procedure:FALSE>
;; (EQ TWO ONE) ; #<procedure:FALSE>
;; (EQ TWO TWO) ;  #<procedure:TRUE> 
