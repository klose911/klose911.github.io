(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;; (car (cdr (filter prime?
;;                   (enumerate-interval 10000 1000000))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp ...)
     (lambda () exp ...))))

(define (force delayed-object)
  (delayed-object))

(define-syntax delay
  (syntax-rules ()
    ((delay exp ...)
     (memo-proc (lambda () exp ...)))))

(define (memo-proc proc)
  (let ((already-run? false)
	(result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

;;; cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define the-empty-stream '())
(define (stream-null? s)
  (eq? s the-empty-stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null?  (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams)) 
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))))) 

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; (stream-enumerate-interval 10000 1000000)
;; (cons-stream 10000 (stream-enumerate-interval 10001 1000000))
;; (cons 10000 (delay (stream-enumerate-interval 10001 1000000)) 

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred 
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; (stream-car
;;  (stream-cdr
;;   (stream-filter prime? (stream-enumerate-interval 10000 1000000)))) ; => 10009 

;; (stream-filter prime?  (cons 10000 (delay (stream-enumerate-interval 10001 1000000)))) 
;; (prime? (stream-car  (cons 10000 (delay (stream-enumerate-interval 10001 1000000))))) 
;; (prime? 10000) ; => #f

;; (stream-filter prime? (stream-cdr (stream-enumerate-interval 10001 1000000)))
;; (stream-filter prime? (force (delay (stream-enumerate-interval 10001 1000000))))
;; (stream-filter prime? (stream-enumerate-interval 10001 1000000))

;; ......

;; (stream-filter prime? (stream-enumerate-interval 10007 1000000))
;; (prime? 10007) ; => #t
;; (cons-stream 10007 (stream-filter prime? stream-cdr (stream-enumerate-interval 10007 1000000)))
;; (cons 10007 (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10007 1000000)))))

;; (stream-cdr (cons 10007 (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10007 1000000))))))
;; (force (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10007 1000000)))))
;; (stream-filter prime? (stream-cdr (stream-enumerate-interval 10007 1000000)))
;; (stream-filter prime? (stream-enumerate-interval 10008 1000000))

;; ....

;; (stream-filter prime? (stream-enumerate-interval 10009 1000000))
;; (prime 10009) ; => #t
;; (cons 10009 (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10009 1000000)))))

;; (stream-car (cons 10009 (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10009 1000000)))))) ; => 10009 

;;;;;;;;;;;;
;; 无穷流 ;;
;;;;;;;;;;;;
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; (stream-car integers) ; => 1
;; (stream-car (stream-cdr integers)) ; => 2 
;; (stream-ref integers 100) ; => 101

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;; (stream-ref no-sevens 100) ; => 117

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fib (fibgen 0 1))

;; (stream-car fib) ; => 0
;; (stream-car (stream-cdr fib)) ; => 1
;; (stream-ref fib 2) ; => 1 
;; (stream-ref fib 20) ; => 6765  

(define (sieve stream)
  (cons-stream
   (stream-car stream) 
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
;; primes
;; => (2 . #<promise:stdin:130:21>)
;; (cons-stream
;;  (stream-car (integers-starting-from 2)) ; 2 
;;  (sieve (stream-filter
;;            (lambda (x)
;;              (not (divisible? x (stream-car (integers-starting-from 2)))))) ; 2 
;; 	(stream-cdr (integers-starting-from 2)))) ; (integers-starting-from 3) 

;; (stream-cdr primes) 
;; (sieve  (stream-filter
;;          (lambda (x)
;;            (not (divisible? x 2))))  
;;         (integers-starting-from 3)) ; 过滤掉所有能被2整除的整数

;; (stream-ref primes 100) ; => 547 

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 基于流运算定义无穷流 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ones (cons-stream 1 ones))

;; ones ; => (1 . #<promise:stdin:130:21>)
;; (stream-car ones) ; => 1 
;; (stream-cdr ones) ; #0=(1 . #<promise!#0#>)
;; (stream-car (stream-cdr ones)) ; => 1
;; (stream-cdr (stream-cdr ones)) ; => #0=(1 . #<promise!#0#>)
;; (stream-car (stream-cdr (stream-cdr ones))) ; => 1

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers))) 

;; integers ; => (1 . #<promise:stdin:130:21>)
;; (stream-car integers) ; => 1
;; (stream-cdr integers) ; =>  (2 . #<promise:stdin:130:21>)
;; (stream-car (stream-cdr integers)) ; => 2
;; (stream-cdr (stream-cdr integers)) ; => (3 . #<promise:stdin:130:21>)
;; (stream-car (stream-cdr (stream-cdr integers))) ; => 3 

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (scale-stream integers 2))

;; (stream-car double) ; => 2
;; (stream-ref double 10) ; => 22 

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

;; fibs ; => (0 . #<promise:stdin:130:21>) 
;; (stream-car fibs) ; => 0
;; (stream-cdr fibs) ; =>  (1 . #<promise:stdin:130:21>)
;; (stream-car (stream-cdr fibs)) ; => 1
;; (stream-cdr (stream-cdr fibs)) ; =>  (1 . #<promise:stdin:130:21>)
;; (stream-cdr (stream-cdr (stream-cdr fibs))) ; => (2 . #<promise:stdin:130:21>)
;; (stream-cdr (stream-cdr (stream-cdr (stream-cdr fibs)))) ; =>  (3 . #<promise:stdin:130:21>)
;; (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr fibs))))) ; => (5 . #<promise:stdin:130:21>)
;; ......
;; (stream-ref fibs 20) ; => 6765 

(define (square x)
  (* x x))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

;; (stream-car primes) ; => 2
;; (stream-cdr primes) ; =>  (3 . #<promise:stdin:130:21>)
;; (stream-car (stream-cdr primes)) ; => 3 
;; (stream-cdr (stream-cdr primes)) ; => (5 . #<promise:stdin:130:21>)
;; (stream-car (stream-cdr (stream-cdr primes))) ; => 5
;; ......
;; (stream-ref primes 100) ; => 547

;;;;;;;;;;;;;;;;;;
;; 流来表示迭代 ;;
;;;;;;;;;;;;;;;;;;
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses) 

;; (define 2-root-stream (sqrt-stream 2))
;; (stream-car 2-root-stream) ; => 1.0
;; (stream-ref 2-root-stream 1) ; => 1.5
;; (stream-ref 2-root-stream 2) ; =>  1.4166666666666665
;; (stream-ref 2-root-stream 3) ; =>1.4142156862745097
;; (stream-ref 2-root-stream 4) ; => 1.4142135623746899
;; (stream-ref 2-root-stream 5) ; => 1.414213562373095
;; (stream-ref 2-root-stream 6) ; => 1.414213562373095

(define (partial-sums s)
  (add-streams  s
		(cons-stream
		 0
		 (partial-sums s))))

;; (stream-car (partial-sums integers)) ; => 1 
;; (stream-ref (partial-sums integers) 4) ; => 15 

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; (stream-car pi-stream) ; => 4.0 
;; (stream-ref pi-stream 1) ; => 2.666666666666667
;; (stream-ref pi-stream 2) ; => 3.466666666666667
;; (stream-ref pi-stream 3) ; => 2.8952380952380956
;; (stream-ref pi-stream 4) ; => 3.3396825396825403
;; (stream-ref pi-stream 5) ; => 2.9760461760461765
;; (stream-ref pi-stream 6) ; =>3.2837384837384844
;; (stream-ref pi-stream 7) ; => 3.017071817071818

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define euler-transform-pi-stream (euler-transform pi-stream))

;; (stream-car euler-transform-pi-stream) ; => 3.166666666666667 
;; (stream-ref euler-transform-pi-stream 1) ; => 3.1333333333333337
;; (stream-ref euler-transform-pi-stream 2) ; => 3.1452380952380956
;; (stream-ref euler-transform-pi-stream 3) ; => 3.13968253968254
;; (stream-ref euler-transform-pi-stream 4) ; => 3.1427128427128435
;; (stream-ref euler-transform-pi-stream 5) ; => 3.1408813408813416 
;; (stream-ref euler-transform-pi-stream 6) ; => 3.142071817071818
;; (stream-ref euler-transform-pi-stream 7) ; => 3.1412548236077655

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define accelerated-pi-stream (accelerated-sequence euler-transform pi-stream))

;; (stream-car accelerated-pi-stream) ; => 4.0 
;; (stream-ref accelerated-pi-stream 1) ; => 3.166666666666667
;; (stream-ref accelerated-pi-stream 2) ; => 3.142105263157895
;; (stream-ref accelerated-pi-stream 3) ; => 3.141599357319005
;; (stream-ref accelerated-pi-stream 4) ; => 3.1415927140337785
;; (stream-ref accelerated-pi-stream 5) ; => 3.1415926539752927
;; (stream-ref accelerated-pi-stream 6) ; => 3.1415926535911765
;; (stream-ref accelerated-pi-stream 7) ; => 3.141592653589778 

;;;;;;;;;;;;;;;;
;; 无穷序对流 ;;
;;;;;;;;;;;;;;;;

(define nil '()) 
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high)))) 
;; (define (flatmap proc seq)
;;   (accumulate append nil (map proc seq)))

;; (define (prime-sum? pair)
;;   (prime? (+ (car pair) (cadr pair))))

;; (define (make-pair-sum pair)
;;   (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; (define (prime-sum-pairs n)
;;   (map make-pair-sum
;;        (filter prime-sum?
;;                (flatmap
;;                 (lambda (i)
;;                   (map (lambda (j) (list i j))
;;                        (enumerate-interval 1 (- i 1))))
;;                 (enumerate-interval 1 n)))))


;; (flatmap
;;  (lambda (i)
;;    (map (lambda (j) (list i j))
;; 	(enumerate-interval 1 (- i 1)))) 
;;  (enumerate-interval 1 10)) 

;; (map (lambda (j) (list 3 j))
;;      (enumerate-interval 1 (- 3 1))) ; => ((3 1) (3 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这样做不行！因为第一个流无穷长，第二个流的元素永远不出现 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1))))) ;; 交错执行 s1, s2 流的 stream-car 和 stream-cdr 

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))
(define sum-prime-pair-stream (stream-filter (lambda (pair)
					       (prime? (+ (car pair) (cadr pair))))
					     int-pairs)) 

;; (stream-car sum-prime-pair-stream) ; => (1 1)
;; (stream-ref sum-prime-pair-stream 1) ; => (1 2)
;; (stream-ref sum-prime-pair-stream 2) ; => (2 3)
;; (stream-ref sum-prime-pair-stream 3) ; => (1 4)
;; ....
;; (stream-ref sum-prime-pair-stream 10) ; => (1 16)

;;; integrand： 被积分函数
;;; initial-value： 初始值
;;; dt：时间增量
;;; 输入流 integrand 经 dt 缩放送入加法器， 加法器输出反馈回来 送入同一个加法器，形成一个反馈循环
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt) 
                              int)))
  int)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 本过程无法工作：定义y 用到 dy ，而当时 dy 还没有定义 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (solve f y0 dt)
;;   (define y (integral dy y0 dt))
;;   (define dy (stream-map f y))
;;   y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand))) ;; 手动求值积分对象 
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt)) ;; 延迟求值 dy 
  (define dy (stream-map f y))
  y)

;; dy / dt = y , y(0) = 1
;; y = e ^ t
;; y(1000) = e ^ (0.0001 * 1000) = e 
(stream-ref (solve (lambda (y) y) 1 0.001) 1000) ; => 2.716923932235896 

;;;;;;;;;;;;;;;;;;
;; 蒙特卡罗模拟 ;;
;;;;;;;;;;;;;;;;;;
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-init 7) 
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;; (estimate-pi 100) 

;;; random stream 
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

;; (stream-car random-numbers) ; => 7
;; (rand-update 7) ; => 88 
;; (stream-ref random-numbers 1) ; => 88
;; (rand-update 88) ; => 116
;; (stream-ref random-numbers 2) ; => 116 
;; (stream-ref random-numbers 3) ; => 110

;; 对一个流的相邻的元素进行运算
;; f: 运算函数
;; s: 流
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

;; (define successive-sum-stream (map-successive-pairs + integers))
;; (stream-car successive-sum-stream) ; => 1 + 2 = 3 
;; (stream-ref successive-sum-stream 1) ; => 3 + 4 = 7
;; (stream-ref successive-sum-stream 2) ; => 5 + 6 = 11 

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

;; (stream-car cesaro-stream) ; => #t 
;;  (= (gcd 7 88) 1) ; => #t 
;; (stream-ref cesaro-stream 1) ; => #f
;; (= (gcd 116 110) 1) ; => #f

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed) ;; 每次都基于已经求值的结果，算出一个新的估计值
    (cons-stream
     (/ passed (+ passed failed)) ;; 计算新的估计值
     (monte-carlo
      (stream-cdr experiment-stream) passed failed))) ;; 延迟计算，只是保存操作
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define monte-carlo-stream (stream-map (lambda (p) (sqrt (/ 6 p)))
				       (monte-carlo cesaro-stream 0 0)))

;; (stream-car monte-carlo-stream) ; =>  2.449489742783178
;; (stream-ref monte-carlo-stream 1) ; =>  3.4641016151377544 
;; (stream-ref monte-carlo-stream 2) ; => 3 
;; (stream-ref monte-carlo-stream 3) ; => 2.8284271247461903 
;; (stream-ref monte-carlo-stream 4) ; => 3.1622776601683795
;; (stream-ref monte-carlo-stream 5) ; => 3 
;; (stream-ref monte-carlo-stream 100) ; =>  3.232379288089343


(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))


;; (define balance-stream (stream-withdraw 100 ones)) 
;; 时刻0，余额是100块
;; (stream-car balance-stream) 
;; 时刻1， 取1块，余额是99块
;; (stream-ref amount-stream-withdrew 1) ; => 99
;; 时刻2，再取1块，余额是98块
;; (stream-ref amount-stream-withdrew 2) ; => 98
;; ...
;; (stream-ref amount-stream-withdrew 100) ; => 0
