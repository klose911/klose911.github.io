(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
	  (lambda ()
	    (error "amb tree exhausted")))))

(initialize-amb-fail)

(define call/cc call-with-current-continuation)
;; (call/cc (lambda (k) (+ 1 2))) ;; call/cc 的参数是一个函数，这个函数只有一个参数，调用 call/cc 会调用这个函数，然后把当前的续延传入这个函数

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function for nondeterminism ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (choose . ls) ; . 表示可变参数
  (if (null? ls) ; 如果没有选择：那调用 amb-fail 续延打印失败消息
      (amb-fail) 
      (let ((prev-amb-fail amb-fail)) ;  保存 amb-fail 到一个局部变量 prev-amb-fail
	(call/cc ; 调用当前续延cc
	 (lambda (cc) 
           (set! amb-fail ; 创建一个新的匿名函数并赋值给 amb-fail
                 (lambda ()
                   (set! amb-fail prev-amb-fail) ; 恢复 amb-fail 为保存的 prev-amb-fail
                   (cc (apply choose (cdr ls))))) ; 在保存的续延 cc 中递归求值接下来的表达式 
           (cc (car ls))))))) ; 在当前续延cc内求值第一个选择，无论求值是否成功，都直接返回

;;; 平方
(define (sq x)
  (* x x))

;;; 勾股定律
(define (pythag a b c)
  (if (= (+ (sq a) (sq b)) (sq c))
      (list a b c)
      (choose)))

;; (pythag (choose 1 2 3) (choose 3 4 5) (choose  4 5 6))
					; => (3 4 5)

(define-syntax amb
  (syntax-rules ()
    ((_) (amb-fail))
    ((_ a) a)
    ((_ a b ...)
     (let ((prev-amb-fail amb-fail)) ; 把全局变量 amb-fail 赋值给 prev-amb-fail 供回溯 // 续延1 
       (call/cc ;调用下面的匿名函数 lambda (k) ...  
	(lambda (k) ; 续延1 作为参数 k 传入
	  (set! amb-fail ; 设置全局变量 amb-fail 为下面匿名函数
		(lambda () ; 如果 (k a) 调用失败，会调用下面的函数
		  (set! amb-fail prev-amb-fail) ; 恢复全局变量 amb-fail 为续延1时候的值
		  (k (amb b ...)))) ; 在续延1 时候求值 b 表达式
	  (k a))))))) ; 续延1 时候求值 a 表达式，如果求值失败，调用 amb-fail，


;; (amb 1 2 3) ; => 1
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; 每调用一次 amb 都会触发 (amb-fail) 的调用，转而调用宏中的 (k (amb b) ...) ，这在保存的续延1中去求值下一个表达式 ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (amb) ; => 2
;; (amb) ; => 3
;; (amb) ;amb tree exhausted

;; (if (amb (> 1 2) (< 2 1) (> 5 1))
;;     1
;;     (amb)) 
;; => 1 

;;;;;;;;;;;;;;;;;;
;; 辅助逻辑函数 ;;
;;;;;;;;;;;;;;;;;;
;;; 确保某个谓词必须为真
(define (require p)
  (if (not p) (amb)))

;;; 没有重复的元素
(define (distinct? . ls)
  (let loop ((lst (car ls)))
    (let ((first (car lst)) (rest (cdr lst)))
      (cond 
       ((null? rest) #t)
       ((member first rest) #f)
       (else (loop rest))))))

;;; 初始化
(initialize-amb-fail)

;;; 楼层问题
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith))) ; 没有人住在同一层
    (require (not (= baker 5))) ; baker 不住在 第5层
    (require (not (= cooper 1))) ; cooper 不住在 第1层
    (require (not (= fletcher 5))) ; fletcher 不住在 第5层
    (require (not (= fletcher 1))) ; fletcher 不住在 第1层
    (require (> miller cooper)) ; miller 比 cooper 住得高
    (require (not (= (abs (- smith fletcher)) 1))) ; smith 和 fletcher 不住在相邻的两层
    (require (not (= (abs (- fletcher cooper)) 1))) ; fletcher 和 cooper 不住在相邻的两层
    (list (list 'baker baker) ; 输出结果
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; (multiple-dwelling) 
;; => ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; (amb-fail)
;; => ;amb tree exhausted

