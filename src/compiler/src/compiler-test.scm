(load "compiler")

(compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next)

;; ((env)
;;  (val)
;;  ( ;; construct the procedure and skip over code for the procedure body
;;   (assign val
;; 	  (op make-compiled-procedure) (label entry2) (reg env))
;;   (goto (label after-lambda1)) 
;;   entry2 ;;  calls to factorial will enter here
;;   (assign env (op compiled-procedure-env) (reg proc))
;;   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;   ;; begin actual procedure body
;;   (save continue)
;;   (save env)
;;   ;; compute (= n 1)
;;   (assign proc (op lookup-variable-value) (const =) (reg env)) ;; "=" 对应的运算对象放入 proc 寄存器
;;   (assign val (const 1)) ;; 1 -> val 
;;   (assign argl (op list) (reg val)) ;; (1) -> argl 
;;   (assign val (op lookup-variable-value) (const n) (reg env)) ;; n -> val 
;;   (assign argl (op cons) (reg val) (reg argl)) ;;  (1, n) -> argl 
;;   (test (op primitive-procedure?) (reg proc)) ;; 测试 proc 是否为原始过程
;;   (branch (label primitive-branch17)) ;; 如果是原始过程的话，跳转到 primitive-branch17 去
;;   compiled-branch16 ;; 实际上这里不会被执行，可以优化
;;   (assign continue (label after-call15)) 
;;   (assign val (op compiled-procedure-entry) (reg proc)) 
;;   (goto (reg val))
;;   primitive-branch17 
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ;;调用结果 (= n 1) -> val 
;;   after-call15 ;;  val now contains result of (= n 1)

;;   (restore env)
;;   (restore continue)

;;   (test (op false?) (reg val)) ;; 测试 (= n 1) 是否为假
;;   (branch (label false-branch4)) ;; 假的话，继续下次递归调用
;;   true-branch5 ;; return 1 
;;   (assign val (const 1))
;;   (goto (reg continue))

;;   false-branch4
;;   ;; compute and return (* (factorial (- n 1)) n) 
;;   (assign proc (op lookup-variable-value) (const *) (reg env)) ;; *运算符 -> proc
;;   (save continue) ;; continue 入栈
;;   (save proc) ;; proc 入栈
;;   (assign val (op lookup-variable-value) (const n) (reg env)) ;; n 常量 -> val 
;;   (assign argl (op list) (reg val)) ;; (n) -> argl 
;;   (save argl) ;; argl 入栈

;;   ;; compute (factorial (- n 1)), which is the other argument for *
;;   (assign proc (op lookup-variable-value) (const factorial) (reg env)) ;; factorial 运算符号 -> proc 
;;   (save proc) ;; proc入栈
;;   ;;  compute (- n 1), which is the argument for factorial 
;;   (assign proc (op lookup-variable-value) (const -) (reg env)) ;; - 运算符 -> proc 
;;   (assign val (const 1)) ;; 1 -> val
;;   (assign argl (op list) (reg val)) ;; (1) -> argl 
;;   (assign val (op lookup-variable-value) (const n) (reg env)) ;; const n -> val 
;;   (assign argl (op cons) (reg val) (reg argl)) ;; (1 n) -> argl 
;;   (test (op primitive-procedure?) (reg proc)) ;; 测试是不是原始的运算符
;;   (branch (label primitive-branch8)) ;; - 是原始的运算符，所以这里跳转到 primitive-branch8 
;;   compiled-branch7
;;   (assign continue (label after-call6))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;   primitive-branch8
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ;; 计算 (- n 1)

;;   after-call6 ;; val now contains result of (- n 1)
;;   (assign argl (op list) (reg val)) ;; ((- n 1)) -> argl 
;;   (restore proc) ;; proc 出栈：factorial 运算符号 -> proc
;;   ;; apply factorial 
;;   (test (op primitive-procedure?) (reg proc)) 
;;   (branch (label primitive-branch11))
;;   compiled-branch10
;;   (assign continue (label after-call9)) ;; after-call9 -> continue 
;;   (assign val (op compiled-procedure-entry) (reg proc)) ;; factorial 的运算入口点 -> val 
;;   (goto (reg val)) ;; 执行 factorial的运算入口点，相当于执行 (factorial (- 1 n)) 
;;   primitive-branch11
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

;;   after-call9 ;;  val now contains result of (factorial (- n 1))  
;;   (restore argl) ;; 从栈中恢复 (n)  -> argl 
;;   (assign argl (op cons) (reg val) (reg argl)) ;; (n (factorial (- n 1))) -> argl 
;;   (restore proc) ;; 从栈中恢复 (* 运算符) -> proc 
;;   (restore continue) ;; 恢复 continue (next) -> proc 
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch14))
;;   compiled-branch13
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;   primitive-branch14
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ;; 执行 (* n (factorial (- n 1)))  
;;   (goto (reg continue))
;;   after-call12
;;   after-if3
;;   after-lambda1
;;   (perform (op define-variable!) (const factorial) (reg val) (reg env))
;;   (assign val (const ok))))
