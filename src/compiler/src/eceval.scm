(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read)

   ;;operations in syntax.scm
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

   ;;operations in eceval-support.scm
   (list 'true? true?)
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
   (list 'get-global-environment get-global-environment))
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义显示求值器模型 ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define eceval
  (make-machine
   '(exp env val proc argl continue unev) ;; 7个寄存器
   eceval-operations ;; scheme 原生实现的操作
   ;; 要汇编的指令集代码
   '(
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; 显示求值器的 repl 循环 ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     read-eval-print-loop
     (perform (op initialize-stack)) ;; 初始化栈
     (perform
      (op prompt-for-input) (const ";;; EC-Eval input:")) ;; 打印提示语
     (assign exp (op read)) ;; 读取输入 -> exp 寄存器
     (assign env (op get-global-environment)) ;; 读取 global env -> env 寄存器
     (assign continue (label print-result)) ;; print-result 指令标号 -> continue 寄存器
     (goto (label eval-dispatch)) ;; 赚取执行 eval-dispatch 对 env 寄存器中的表达式进行求值
     ;;; 打印求值结果
     print-result 
     (perform
      (op announce-output) (const ";;; EC-Eval value:")) ;; 打印提示语
     (perform (op user-print) (reg val)) ;; 调用 user-print 真实打印 val寄存器中的求值结果
     (goto (label read-eval-print-loop)) ;; 重新开始 repl 循环
     ;;; 未知表达式类型
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))
     ;;; 未知过程类型
     unknown-procedure-type
     (restore continue)    ; clean up stack (from apply-dispatch)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))
     ;;; 打印出错类型
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop)) ;;; 回到基本求值循环时将栈置空，重新开始新一次循环
     ;;;;;;;;;;;;;;;;;;;;
     ;; 求值器核心代码 ;;
     ;;;;;;;;;;;;;;;;;;;;
     eval-dispatch 
     (test (op self-evaluating?) (reg exp)) ;; 自求值表达式 
     (branch (label ev-self-eval)) 
     (test (op variable?) (reg exp)) ;; 变量表达式 
     (branch (label ev-variable))
     (test (op quoted?) (reg exp)) ;; 引用表达式
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp)) ;; 赋值表达式
     (branch (label ev-assignment))
     (test (op definition?) (reg exp)) ;; 定义表达式
     (branch (label ev-definition))
     (test (op if?) (reg exp)) ;; 条件表达式
     (branch (label ev-if))
     (test (op lambda?) (reg exp)) ;; lambda 表达式
     (branch (label ev-lambda))
     (test (op begin?) (reg exp)) ;; begin 表达式
     (branch (label ev-begin))
     (test (op application?) (reg exp)) ;; 过程表达式
     (branch (label ev-application))
     (goto (label unknown-expression-type)) ;; 无法求值
     ;;;;;;;;;;;;;;;;;;;;
     ;; 简单表达式求值 ;;
     ;;;;;;;;;;;;;;;;;;;;
     ev-self-eval
     ;; exp 寄存器的值直接放入 val 寄存器
     (assign val (reg exp)) 
     (goto (reg continue))

     ev-variable
     ;; env 环境中查找 exp 变量，结果放入 val 寄存器
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     ;; exp 寄存器中的表达式 作为参数调用 text-of-quotation 过程，把值放入到 val 寄存器作为结果
     (assign val (op text-of-quotation) (reg exp)) 
     (goto (reg continue))

     ev-lambda
     ;; exp 表达式中获得形参表放入到 unev 寄存器
     (assign unev (op lambda-parameters) (reg exp))
     ;; exp 表达式中获得过程体放入到 exp 寄存器
     (assign exp (op lambda-body) (reg exp))
     ;; 形参表，过程体，当前环境调用 make-procedure 过程，创建一个新的匿名过程，放入到 val 寄存器
     (assign val (op make-procedure)
             (reg unev) (reg exp) (reg env))
     (goto (reg continue))
     ;;;;;;;;;;;;;;;;;;;;
     ;; 过程应用的求值 ;;
     ;;;;;;;;;;;;;;;;;;;;
     
     ;;; 过程应用的求值入口
     ;;; exp 寄存器存放“运算符表达式”和“实参表达式”
     ev-application
     (save continue) ;; 保存续点，因为之后要跳转到 continue 继续执行
     (save env) ;; 保存环境，因为后面对 operands 求值需要
     (assign unev (op operands) (reg exp)) ;; 把 operands 表达式缓存到 unev 
     (save unev) ;; 保存 operands 表达式
     (assign exp (op operator) (reg exp)) ;; 把运算符 operator 放入到 exp寄存器 
     (assign continue (label ev-appl-did-operator)) ;; 对运算符求值后，转去执行 ' ev-appl-did-operator' 
     (goto (label eval-dispatch)) ;; 调用 eval-dispatch 来对运算符进行求值
     ;;; 对某个实参表达式进行求值
     ev-appl-did-operator
     (restore unev)  ;; 恢复过程参数表                
     (restore env) ;; 恢复环境表
     (assign argl (op empty-arglist)) ;; argl 寄存器初始化为空列表
     (assign proc (reg val))  ;; 将运算符过程存入 proc 寄存器
     (test (op no-operands?) (reg unev)) ;; 测试过程参数表是否为空
     (branch (label apply-dispatch)) ;; 如果过程参数表为空，则立刻调用 proc 运算符
     (save proc) ;; 保存求出的运算符过程，而后向下求值运算对象
     ev-appl-operand-loop
     (save argl) ;; 保存实参表
     (assign exp (op first-operand) (reg unev)) ;; 取出第一个运算对象表达式 -> exp 寄存器
     (test (op last-operand?) (reg unev)) ;; 测试这个表达式是否是最后一个表达式
     (branch (label ev-appl-last-arg)) ;; 如果是转而执行 ev-appl-last-arg 
     (save env) ;; 保存当前环境
     (save unev) ;; 保存“整个实参表达式”组成的表
     (assign continue (label ev-appl-accumulate-arg)) ;; 续点设置为 "ev-appl-accumulate-arg"，累加求出的实参值
     (goto (label eval-dispatch)) ;; 求值第一个运算对象
     ;;; 累加求值后的实参值
     ev-appl-accumulate-arg
     (restore unev) ;; 恢复保存的实参表达式组成的表 -> unev 
     (restore env) ;; 恢复环境 -> env 
     (restore argl) ;; 恢复保存的已经求值过的实参值组成的表 -> argl 
     (assign argl (op adjoin-arg) (reg val) (reg argl)) ;; 把求出的实参值添加到“已经求值的实参值组成的表” -> argl 寄存器
     (assign unev (op rest-operands) (reg unev)) ;; “实参表达式组成的表”中去掉已经刚才求值的表达式 -> unev 寄存器
     (goto (label ev-appl-operand-loop)) ;; 求值下一个实参表达式
     ;;; 求值最后一个实参表达式
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg)) ;; 求值完最后一个运算对象后转到 'ev-appl-accum-last-arg' 
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg 
     (restore argl) ;; 恢复“已经求值的实参值表“ -> argl 
     (assign argl (op adjoin-arg) (reg val) (reg argl)) ;; 把最后一个计算出来的实参值添加到“已经求值的实参值表“  -> argl 
     (restore proc) ;; 恢复求值过的运算符对象 -> proc 
     (goto (label apply-dispatch)) ;; 调用 apply 过程
     ;;; 实际应用的过程
     apply-dispatch
     (test (op primitive-procedure?) (reg proc)) ;; 测试是否是基本过程
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))  ;; 测试是否是复合过程
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))
     ;;; 应用基本过程
     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))
     ;;; 应用复合过程
     compound-apply 
     (assign unev (op procedure-parameters) (reg proc)) ;; 取出运算符对象的形参表 -> unev 
     (assign env (op procedure-environment) (reg proc)) ;; 取出运算符对象的环境 -> env 
     (assign env (op extend-environment) 
             (reg unev) (reg argl) (reg env)) ;; 扩充运算符对象的环境：绑定形参表和实参表
     (assign unev (op procedure-body) (reg proc)) ;; 取出运算符对象的过程体 -> unev 
     (goto (label ev-sequence)) ;; 调用“序列求值”代码入口
     ;;;;;;;;;;;;;;;;;;;;
     ;; 序列表达式求值 ;;
     ;;;;;;;;;;;;;;;;;;;;
     ;;; begin 表达式的入口
     ev-begin
     (assign unev (op begin-actions) (reg exp)) ;; 取出 begin 的实际序列
     (save continue)  ;;  保存求值完的继续点，与其他入口一致
     (goto (label ev-sequence))
     ;;; 序列表达式求值入口：支持尾递归！！！
     ev-sequence ;; 此时 unev 寄存器中是待求值的表达式序列
     (assign exp (op first-exp) (reg unev)) ;; 取出序列中第一个表达式 
     (test (op last-exp?) (reg unev)) ;; 测试是否是最后一个表达式
     (branch (label ev-sequence-last-exp)) ;; 跳转到最后一个表达式的特殊处理 ev-sequence-last-exp 
     (save unev) ;; 保存表达式序列
     (save env) ;; 保存执行表达式的环境
     (assign continue (label ev-sequence-continue)) ;; 设置求完当前表达式后的续点 ev-sequence-continue （求值余下的表达式） 
     (goto (label eval-dispatch)) ;; 求值当前表达式
     ;;; 当前表达式求值完毕
     ev-sequence-continue 
     (restore env) ;; 恢复原来的环境
     (restore unev) ;; 恢复表达式列表
     (assign unev (op rest-exps) (reg unev)) ;; 从表达式列表去掉已经求值过的表达式 -> unev 寄存器
     (goto (label ev-sequence)) ;; 继续执行余下表达式序列求值
     ;;; 做序列中最后一个表达式的求值
     ev-sequence-last-exp 
     (restore continue) ;;  恢复续点寄存器（调用 ev-sequence 前的 continue 寄存器中的值）
     (goto (label eval-dispatch)) ;; 求值最后一个表达式
     ;;;;;;;;;;;;;;;;;;;;
     ;; 条件表达式求值 ;;
     ;;;;;;;;;;;;;;;;;;;;
     ;;; 条件表达式入口
     ev-if
     (save exp)                    ; 保存整个 if 表达式供后面使用
     (save env)
     (save continue)
     (assign continue (label ev-if-decide)) ;; 谓词求值后，转而执行 ev-if-decide
     (assign exp (op if-predicate) (reg exp)) ;; 获得谓词表达式
     (goto (label eval-dispatch))  ; 对谓词进行求值
     ;;; 根据谓词求值结果跳转
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val)) ;; ; 检测结果是否为真
     (branch (label ev-if-consequent)) ;; 是真的时候，赚取执行 ev-if-consequent 
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp)) ;; 获得 if 表达式中“谓词为假”对应的表达式 -> exp 
     (goto (label eval-dispatch)) ;; 对 alternative 表达式进行求值
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp)) ;; 获得 if 表达式中“谓词为真”对应的表达式 -> exp 
     (goto (label eval-dispatch)) ;; 对 consequent 表达式进行求值
     ;;;;;;;;;;;;;;;;;;;;
     ;; 赋值表达式求值 ;;
     ;;;;;;;;;;;;;;;;;;;;
     ev-assignment
     (assign unev (op assignment-variable) (reg exp)) ;; 赋值表达式的变量名 -> unev 寄存器
     (save unev)                   ;; 变量名压栈为以后使用
     (assign exp (op assignment-value) (reg exp)) ;; 赋值表达式的“值表达式” -> exp 寄存器
     (save env) ;; 环境入栈
     (save continue) ;; 续点压栈
     (assign continue (label ev-assignment-1)) ;; “变量值表达式”求值以后转而执行 ev-assignment-1 
     (goto (label eval-dispatch))  ;; 对值表达式进行求值
     ev-assignment-1
     (restore continue) ;; 续点恢复
     (restore env) ;; 环境恢复
     (restore unev) ;; 变量恢复
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env)) ;; 执行赋值操作(set-variable-value!) ，修改环境中的变量绑定
     (assign val (const ok)) ;; 常量"ok" -> 结果寄存器 val 
     (goto (reg continue)) ;; 继续执行调用 ev-assignment 前的续点
     ;;;;;;;;;;;;;;;;;;;;
     ;; 定义表达式求值 ;;
     ;;;;;;;;;;;;;;;;;;;;
     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)                   
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))  
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env)) ;; 调用 scheme 实现的定义操作(define-variable!) 实现在环境中定义变量
     (assign val (const ok))
     (goto (reg continue))
     ))) 

'(EXPLICIT CONTROL EVALUATOR LOADED)
