(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value) (set! contents value)))
	    (else
	     (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; (define test-register (make-register 'test))
;; (get-contents test-register) ;; *unassigned*
;; (set-contents! test-register 10)
;; (get-contents test-register) ;; 10

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;; (define test-stack (make-stack))
;; (pop test-stack) ;;  Empty stack -- POP
;; (push test-stack 1)
;; (pop test-stack) ;;  1
;; (pop test-stack) ;;  Empty stack -- POP
;; (push test-stack 2) ;;
;; (test-stack 'initialize) ;; done
;; (pop test-stack) ;;  Empty stack -- POP

(define (make-new-machine)
  (let ((pc (make-register 'pc)) ;; 指令寄存器
	(flag (make-register 'flag)) ;; 标志寄存器
	(stack (make-stack)) ;; 栈
	(the-instruction-sequence '())) ;; 指令列表
    (let ((the-ops ;; 操作列表
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))
		 ;; 增加一个新的打印栈统计信息的操作
		 (list 'print-stack-statistics
		       (lambda () (stack 'print-statistics)))))
	  (register-table ;; 寄存器列表
	   (list (list 'pc pc) (list 'flag flag))))
      ;; 添加新的寄存器
      (define (allocate-register name) 
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      ;; 从寄存器列表获得特定寄存器
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      ;; 执行指令
      (define (execute)
	(let ((insts (get-contents pc))) ;; 获得 pc 寄存器的值
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start) ;; 启动机器
	       (set-contents! pc the-instruction-sequence) ;; pc 寄存器指向指令列表
	       (execute)) ;; 执行指令
	      ((eq? message 'install-instruction-sequence) ;; 安装指令列表 
	       (lambda (seq) (set! the-instruction-sequence seq))) 
	      ((eq? message 'allocate-register) allocate-register) ;; 添加寄存器
	      ((eq? message 'get-register) lookup-register) ;; 查询寄存器
	      ((eq? message 'install-operations) ;; 安装操作过程
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack) ;; 返回栈
	      ((eq? message 'operations) the-ops) ;; 返回操作列表
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; 启动机器
(define (start machine)
  (machine 'start))

;; 获得寄存器中的值
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

;; 设置寄存器中的值
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

;; 取指定寄存器信息
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; 定义寄存器机器
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
	      register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))


;;;;;;;;;;;;
;; 指令表 ;;
;;;;;;;;;;;;

;; 构造指令表
(define (make-instruction text)
  (cons text '())) ;; 构造指令表时，执行过程暂时用一个空表，后面将填入实际执行过程

;; 获取指令
(define (instruction-text inst)
  (car inst))
;; 获得指令执行过程
(define (instruction-execution-proc inst)
  (cdr inst))
;; 设置指令执行过程
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

;; (define test-b-inst (make-instruction 'test-b))
;; (instruction-text test-b-inst) ;; test-b
;; (instruction-execution-proc test-b-inst) ;; ()
;; (set-instruction-execution-proc! test-b-inst '(assign test)) 
;; (instruction-execution-proc test-b-inst) ;; => (assign test)

;;;;;;;;;;;;
;; 标号表 ;;
;;;;;;;;;;;;

;; 把标号和指令做关联
(define (make-label-entry label-name insts)
  (cons label-name insts)) ;; 标号表项就是序对 

;; (define test-b-lable (make-label-entry 'test-b test-b-inst)) 

;; 查询某个标号关联表下某个标号对应的指令
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;;;;;;;;;;;;
;; 汇编器 ;;
;;;;;;;;;;;;
(define (assemble controller-text machine)
  (extract-labels controller-text ;; 构造初始指令表和标号表
                  (lambda (insts labels) ;; 指令表，标号表作为参数
                    (update-insts! insts labels machine) ;; 以指令表、标号表和机器为参数，生成各条指令的执行过程加入指令表
                    insts))) ;; 返回指令表

;; 逐项检查指令表内容，提取其中的标号
;; text: 控制器代码
;; receive: 函数参数 
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      ;; 递归处理控制器正文序列的 cdr
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text))) 
			  (if (symbol? next-inst) ;; 检查 car 是否是标号
			      (receive insts ;; 如果是标号，加入标号项
				  (cons (make-label-entry next-inst
							  insts)
					labels))
			      (receive (cons (make-instruction next-inst)
					     insts)  ;; 反之加入指令表项
				  labels)))))))

;;; 原来每个位置只有指令正文，执行过程用空表占位，现在加入实际的执行过程
;;; insts: 指令表
;;; labels: 标号关联表
;;; machine: 机器模型
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each ;; 给一条指令设置执行过程
     (lambda (inst)
       (set-instruction-execution-proc! 
	inst
	(make-execution-procedure ;; 构造一条指令的执行过程
	 (instruction-text inst) labels machine
	 pc flag stack ops)))
     insts)))

;;;;;;;;;;;;;;;;;;
;; 生成执行过程 ;;
;;;;;;;;;;;;;;;;;;
;;; 生成一条指令的执行过程
;;; inst: 指令
;;; labels: 标号表
;;; machine: 机器模型
;;; pc: 指令寄存器
;;; flag: 标志寄存器
;;; stack: 栈
;;; ops: 操作表
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; assign instruction ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; 为asssign指令构造执行过程
(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst))) ;; 从指令中取出被赋值的寄存器
        (value-exp (assign-value-exp inst))) ;; 从指令中取出被赋值的值表达式
    (let ((value-proc ;; 求值的执行过程
           (if (operation-exp? value-exp)
	       (make-operation-exp
                value-exp machine labels operations) ;; 构造一般 op 表达式的执行过程
	       (make-primitive-exp
                (car value-exp) machine labels)))) ;; 构造基本表达式的 执行过程。基本表达式包括 reg, label, const
      (lambda ()                ; assign 的执行过程
        (set-contents! target (value-proc)) ;; 调用 value-proc 过程，并把结果赋值给对应的寄存器
        (advance-pc pc))))) ;; pc 寄存器自增 

;;; 获得 assign 指令中的寄存器表达式
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))


;;; 获得 assign 指令中的赋值表达式
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

;;; 通用的指令计数器的更新过程
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;;;;;;;;;;;;;;;;;;;;;;;
;; test instruction  ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; 生成 test 指令的执行过程
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst))) ;; 获得条件的求值表达式
    (if (operation-exp? condition)
        (let ((condition-proc 
	       (make-operation-exp ;; 产生条件的求值过程
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc)) ;; 调用 condition-proc 过程，把结果设置到 flag 寄存器
            (advance-pc pc))) ;; 更新 pc 寄存器
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; branch instruction  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 生成 branch 指令的执行过程
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst))) ;; 获取转跳指令里的标号
    (if (label-exp? dest)
	(let ((insts (lookup-label labels (label-exp-label dest)))) ;; 从标号表里找出标号在指令序列里的位置
	  (lambda ()
	    (if (get-contents flag) ;; 根据 flag 的值决定如何更新 pc
		(set-contents! pc insts) ;; flag 为真，则把指令寄存器更新为标号在指令序列中的位置
		(advance-pc pc)))) ;; flag 为假，按照通用方式更新指令寄存器
	(error "Bad BRANCH instruction -- ASSEMBLE" inst))))


(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;;;;;;;;;;;;;;;;;;;;;;;
;; goto instruction  ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; 生成 goto 指令的执行过程
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst))) ;; 获取转跳指令里的目的
    (cond ((label-exp? dest) ;; 标号处理类似于 branch 
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest) ;; 寄存器间接跳转
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest)))) ;; 从机器寄存器表中获得对应的寄存器变量
             (lambda ()
	       (set-contents! pc (get-contents reg))))) ;; 从寄存器变量中获得对应的值，并把值赋给指令寄存器 pc 
          (else (error "Bad GOTO instruction -- ASSEMBLE"
		       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;;;;;;;;;;;;;;;;;;;;;;;
;; stack instruction ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; 生成 save 指令的执行过程（寄存器中的内容压栈）
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

;;; 生成 restore 指令的执行过程（栈上的内容出栈到寄存器）
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perform instruction  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 为 perform 指令生成执行过程
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
	(let ((action-proc
	       (make-operation-exp
		action machine labels operations))) ;; 构造 op  表达式的执行过程
	  (lambda ()
	    (action-proc) ;; 执行 op 表达式的执行过程
	    (advance-pc pc))) ;; 更新指令寄存器 pc 
	(error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst)
  (cdr inst))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitive instruction  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 生成基本表达式的执行过程
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c))) ;; 返回常量值
	((label-exp? exp)
	 (let ((insts
		(lookup-label labels
			      (label-exp-label exp)))) 
	   (lambda () insts))) ;; 返回标号在标号指令关联表中对应的指令
	((register-exp? exp)
	 (let ((r (get-register machine
				(register-exp-reg exp)))) ;; 获取寄存器表中的对应寄存器变量
	   (lambda () (get-contents r)))) ;; 返回对应寄存器变量中的内容
	(else
	 (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subprocess instruction  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 生成子表达式的执行过程
;;; exp: 子表达式
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations)) ;; 从操作表中查找对应操作名的函数过程，比如 + , = , remainder等
	(aprocs
	 (map (lambda (e)
		(make-primitive-exp e machine labels))
	      (operation-exp-operands exp)))) ;; 为每个操作的参数对象生成一个执行过程
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs))))) ;; 调用每个操作参数对象的执行过程，得到它们的值；而后应用于操作本身的执行过程

;;; 用操作名到从机器的操作表里查找对应的操作
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
	(cadr val)
	(error "Unknown operation -- ASSEMBLE" symbol))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

;; from 4.1
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; monitor performance ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

'(REGISTER SIMULATOR LOADED)
