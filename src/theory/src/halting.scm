(define (stop? P x)
  ....)

(define (loop-forever x)
  (loop-forever x)) 

;; (stop? loop-forever 1) ; => #f 

(define (diag x)
    (if (stop? x x)
	(loop-forever)
	42))

;; (diag diag)

;; loop-forever 这意味着 (stop? diag diag) 为真，这和stop? 函数的定义不符
;; 如果返回42，则意味着(stop? diag diag) 为false，这又和 stop? 函数的定义不符



