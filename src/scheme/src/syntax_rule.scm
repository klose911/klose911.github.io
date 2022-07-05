;;;;;;;;;;;;
;; 简单宏 ;;
;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; 给某个变量赋值 '() ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax nil!
  (syntax-rules ()
    ;; 转换前和转换后的列表
    ((_ x) ;; 转换前的代码，_ 表示 宏的名字
     (set! x '())))) ;; 转换后的代码

;; (define a 1)
;; a ; => 1
;; (nil! a)
;; a ; => () 

(define (f-nil! x)
  (set! x '())) 

;; (define a 1)
;; a ; => 1
;; (f-nil! a) ; => () 
;; a ; => 1
;; (set! a '())
;; a ; => ()


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 当谓词为真的时候，对接下来的表达式求值 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...) ; ... 含义是任意个表达式，可以是0个
     (if pred (begin b1 ...)))))

;; (let ((i 0))
;;   (when (= i 0)
;;     (display "i == 0")
;;     (newline)))

;; => i == 0
;; ;Unspecified return value


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; while宏：表示条件成立的循环 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))

;; (let ((i 0))
;;   (while (< i 10)
;;     (display i)
;;     (display #\Space)
;;     (set! i (+ i 1))))
;; => 0 1 2 3 4 5 6 7 8 9 
;; ;Unspecified return value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for宏：表示数字在范围之内的循环 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	 b1 ...
	 (loop (1+ i)))))))

;; (for (i 0 10)
;;   (display i)
;;   (display #\Space))
;; => 0 1 2 3 4 5 6 7 8 9 
;; ;Unspecified return value

;;;;;;;;;;;;;;
;; 多种模式 ;;
;;;;;;;;;;;;;;

;; incf宏
(define-syntax incf
  (syntax-rules ()
    ((_ x) (begin (set! x (+ x 1)) x)) ; 如果不给增加参数，默认增加1
    ((_ x i) (begin (set! x (+ x i)) x))))

;; (let ((i 0) (j 0))
;;   (incf i)
;;   (incf j 3)
;;   (display (list 'i '= i))
;;   (newline)
;;   (display (list 'j '= j)))

;; => (i = 1)
;; (j = 3)
;; ;Unspecified return value


;;;;;;;;;;;;;;;;
;; 递归定义宏 ;;
;;;;;;;;;;;;;;;;
(define-syntax my-and
  (syntax-rules ()
    ((_) #t)
    ((_ e) e)
    ((_ e1 e2 ...)
     (if e1
	 (my-and e2 ...)
	 #f))))

;; (my-and) ; => #t 
;; (my-and #f) ; => #f 
;; (my-and (> 2 1)) ; => #t
;; (my-and #t #f) ; => #f
;; (my-and #t (> 2 1)) ; => #t
;; (my-and #t (> 2 1) (< 3 2) (= 1 1))

(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ e) e)
    ((_ e1 e2 ...)
     (let ((t e1))
       (if t t (my-or e2 ...)))))) 

;; (my-or) ; => #f 
;; (my-or #t) ; => #t 
;; (my-or (< 2 1)) ; => #f
;; (my-or #f #f) ; => #f
;; (my-or #f (> 2 1)) ; => #t
;; (my-or #f (> 2 1) (< 3 2) (= 1 1)) ; => #t

;;;;;;;;;;;;;;;;
;; 保留关键字 ;;
;;;;;;;;;;;;;;;;

(define-syntax my-cond
  (syntax-rules (else)
    ((_ (else e1 ...))
     (begin e1 ...))
    ((_ (e1 e2 ...))
     (when e1 e2 ...))
    ((_ (e1 e2 ...) c1 ...)
     (if e1 
	 (begin e2 ...)
	 (cond c1 ...)))))

;; (my-cond (else (+ 1 2))) ; => 3

;; (my-cond ((> 1 0) (+ 1 2))) ; => 3
;; (my-cond ((< 1 0) (+ 1 2))) ; => ;Unspecified return value

;; (my-cond ((< 1 0) (+ 1 2))
;; 	 ((> 1 0) (+ 2 3))) ; => 5 
;; (my-cond ((< 1 0) (+ 1 2))
;; 	 (else (+ 2 3))) ; => 5 
