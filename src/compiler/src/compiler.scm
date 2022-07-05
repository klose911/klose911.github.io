(load "syntax")

;;;;;;;;;;;;;;;;
;; 编译的入口 ;;
;;;;;;;;;;;;;;;;
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;;; 指令序列构造函数
;;; needs: 需要使用的寄存器集合
;;; modifies: 执行是修改的寄存器集合
;;; statements: 语句 
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

;;; 产生空指令序列
(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;;;;;;;;;;;;;;;;;
;; 表达式的编译 ;;
;;;;;;;;;;;;;;;;;;

;;; 编译连接代码
;;; linkage: 连接，return, next, 或者标号
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return) ;; 过程返回
         (make-instruction-sequence '(continue) '() ;; 需要 continue 寄存器，不修改任何寄存器
				    '((goto (reg continue))))) ;; 产生 (goto (reg continue)) 指令
        ((eq? linkage 'next) ;; 下一条语句
         (empty-instruction-sequence)) ;; 不产生任何指令，不需要，也不修改任何寄存器
        (else ;; 连接为标号
         (make-instruction-sequence '() '() ;; 不需要，也不修改任何寄存器
				    `((goto (label ,linkage))))))) ;; 产生跳转到标号的指令

;;; 把连接代码加入到指令序列最后
;;; linkage: 连接代码
;;; instruction-sequence: 指令序列
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue) ;; comile-linkage 产生的指令可能需要 continue
	      instruction-sequence
	      (compile-linkage linkage)))

;;; 编译自求值语句
;;; exp 自求值表达式
;;; target : 目标寄存器
;;; linkage: 连接目标
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '() (list target)
					       `((assign ,target (const ,exp))))))
;;; 编译引用语句
;;; exp: 引用表达式
;;; target : 目标寄存器
;;; linkage: 连接目标
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '() (list target)
					       `((assign ,target (const ,(text-of-quotation exp)))))))

;;; 编译变量语句
;;; exp: 引用表达式
;;; target : 目标寄存器
;;; linkage: 连接目标
(define (compile-variable exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '(env) (list target)
					       `((assign ,target
							 (op lookup-variable-value) (const ,exp) (reg env))))))

;;; 编译赋值语句
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp)) ;; 获取 赋值表达式的变量
        (get-value-code ;; 编译”赋值表达式的求值表达式“为”指令序列“ 
         (compile (assignment-value exp) 'val 'next))) ;; 目标寄存器 val：生成代码把值放入 val ，连接方式 next : 执行随后的语句
    (end-with-linkage linkage
		      (preserving '(env) ;; 所用拼接方式要求维持 env，因为设置变量都需要当时环境，而产生变量值的代码可能是复杂表达式的编译结果，其中完全可能修改 env 寄存器
				  get-value-code
				  (make-instruction-sequence '(env val) (list target)
							     `((perform (op set-variable-value!) ;; 执行真实的赋值操作
									(const ,var)
									(reg val)
									(reg env)) ;; 把,var 作为变量名，把 val寄存器的值（求值表达式计算的结果），绑定在 env 寄存器指向的环境中  
							       (assign ,target (const ok)))))))) ;; 常量 ok 放入 target 目标寄存器 ，作为返回值

;;; 编译定义语句
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
	(get-value-code
	 (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
		      (preserving '(env)
				  get-value-code
				  (make-instruction-sequence '(env val) (list target)
							     `((perform (op define-variable!) ;; 这里调用 define-variable! 
									(const ,var)
									(reg val)
									(reg env))
							       (assign ,target (const ok))))))))

;;; 条件表达式
(define (compile-if exp target linkage)
  ;;; 生成三个新标号
  (let ((t-branch (make-label 'true-branch))
	(f-branch (make-label 'false-branch))                    
	(after-if (make-label 'after-if)))
    (let ((consequent-linkage
	   (if (eq? linkage 'next) after-if linkage))) ;; 根据连接确定 then 最后的连接
      (let ((p-code (compile (if-predicate exp) 'val 'next)) ;; 编译谓词表达式代码，求值的结果放入到 val寄存器，连接的方式：next 
	    (c-code 
	     (compile
	      (if-consequent exp) target consequent-linkage)) ;; 编译谓词为真时候的表达式，目标寄存器是target，使用计算出来的 consequent-linkage作为连接方式
	    (a-code
	     (compile (if-alternative exp) target linkage))) ;; 编译谓词为假时候的表达式，目标寄存器仍为target，连接方式和条件表达式的一样: linkage
	(preserving '(env continue) ;; 求谓次条件的值，前后 env,  continue两个寄存器
		    p-code 
		    (append-instruction-sequences
		     ;; 产生如下指令序列：检查 val 寄存器（谓词计算结果存放在此）是否为假，如果为假则执行 f-branch 对应的标号
		     (make-instruction-sequence '(val) '() 
						`((test (op false?) (reg val))
						  (branch (label ,f-branch)))) 
		     (parallel-instruction-sequences ;; 拼接两段不会同时执行的代码
		      (append-instruction-sequences t-branch c-code) 
		      (append-instruction-sequences f-branch a-code))
		     after-if))))))

;;; 编译序列表达式
(define (compile-sequence seq target linkage)
  (if (last-exp? seq) ;; 测试是否是最后一个子表达式
      (compile (first-exp seq) target linkage) ;; 编译最后一个子表达式，目标寄存器：target，连接：整个序列的连接
      (preserving '(env continue) ;; 需要保留 env (下一个子表达式求值需要)， continue（最后一个拼接需要）
		  (compile (first-exp seq) target 'next) ;; 编译下一个子表达式，目标寄存器：target，连接: next 
		  (compile-sequence (rest-exps seq) target linkage)))) ;; 递归编译余下的子表达式，目标寄存器：target，连接：整个序列的连接

;;; 编译 lambda 表达式
(define (compile-lambda exp target linkage)
  ;; 生成 2个新的标号： proc-entry 和 after-lambda 
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    ;; 计算 lambda表达式的连接
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
			  (make-instruction-sequence '(env) (list target)
						     `((assign ,target
							       (op make-compiled-procedure)
							       (label ,proc-entry)
							       (reg env))))) ;; 构建一个过程对象，标号是 proc-entry的值，环境是 env, 把这个对象赋值到 target 寄存器
        (compile-lambda-body exp proc-entry)) ;; 编译 lambda 过程体
       after-lambda)))) ;; after-lambda 标号

 ;;; 编译 lambda 运行过程
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp))) ;; 获得形参表
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env) ;; 构造过程体需要的寄存器是 env, proc, argl , 构造完成后：修改的寄存器是 env 
				`(,proc-entry ;; 过程体对应的标号
				  (assign env (op compiled-procedure-env) (reg proc)) ;; 调用lambda表达式时候的环境
				  (assign env 
					  (op extend-environment) ;; 扩充环境
					  (const ,formals) ;; 把实参和形参在环境中绑定
					  (reg argl)
					  (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return)))) ;; 编译过程体的指令，执行过程体的目标寄存器是 val, 连接方式: return（直接返回）

;;;;;;;;;;;;;;;;;;;;
;; 过程应用的编译 ;;
;;;;;;;;;;;;;;;;;;;;

;;; 编译过程应用
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next)) ;; 编译运算符表达式: 目标寄存器 proc，连接方式 next
	(operand-codes
	 (map (lambda (operand) (compile operand 'val 'next))
	      (operands exp)))) ;; 依次编译各个运算参数表达式：目标寄存器 val, 连接方式 next
    ;; 整个段前后需要保留和恢复 continue， “过程调用的连接” 需要它
    (preserving '(env continue) ;; 求值运算符前后需要保留和恢复 env：求值运算符时可能修改它们，求值运算对象时需要它们
		proc-code 
		(preserving '(proc continue) ;; 构造实际参数表前后需要保留 proc : 运算对象求值可能修改它，实际过程应用需要它
			    (construct-arglist operand-codes) ;; 将运算对象指令序列与在 argl 里构造实参表的代码组合
			    (compile-procedure-call target linkage))))) ;; 编译过程调用代码

;;; 编译构造实际参数列表
(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes))) ;; 逆向参数顺序，从最后一个实参开始处理
    (if (null? operand-codes) ;; 如果参数表达式表为空
        (make-instruction-sequence '() '(argl)
				   '((assign argl (const ())))) ;; 直接为 argl 构造一个空表
        (let ((code-to-get-last-arg ;; 最后一个实参
               (append-instruction-sequences
                (car operand-codes) ;; 求值最后一个实参值，结果放入到 val 寄存器 
                (make-instruction-sequence '(val) '(argl) ;; 
					   '((assign argl (op list) (reg val))))))) ;; 把 val寄存器中的值放入一个空列表，列表的值赋予给 argl 寄存器
          (if (null? (cdr operand-codes))
              code-to-get-last-arg ;; 只有一个实参，只需要返回 code-to-get-last-arg 
              (preserving '(env) ;; 求值其他的实参时候（除了最后一个参数），需要保存和恢复 env 寄存器：可能会有其他子表达式会修改 env 寄存器
			  code-to-get-last-arg
			  (code-to-get-rest-args (cdr operand-codes)))))))) ;; 继续求值其他实参，并放入 argl 寄存器中的列表

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl) ;; 求值下一个实参的时候（除了第一个参数），必须保存和恢复 argl 寄存器：因为这里面保存了由其他实参值组成的列表
		     (car operand-codes) ;; 执行第一个参数求值
		     (make-instruction-sequence '(val argl) '(argl) 
						'((assign argl
							  (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes)) 
        code-for-next-arg
        (preserving '(env)
		    code-for-next-arg
		    (code-to-get-rest-args (cdr operand-codes))))))

;;; 编译过程调用
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call))) ;; 生成三个标号
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage))) ;; 如果原调用的连接是 next: com
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
				  `((test (op primitive-procedure?) (reg proc))
				    (branch (label ,primitive-branch))))
       (parallel-instruction-sequences ;; 拼接两段不会同时执行的代码
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
			   (make-instruction-sequence '(proc argl)
						      (list target)
						      `((assign ,target
								(op apply-primitive-procedure)
								(reg proc)
								(reg argl))))))) ;; 调用原始过程（scheme实现的）
       after-call))))

;;; 编译“复合过程调用”
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return))) ;; 目标解释器是 val, 并且连接方式不是 return 
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage)) ;; 为执行完毕后设置续点为 整体调用的连接方式（标号或next）
                                      (assign val (op compiled-procedure-entry)
                                              (reg proc)) ;; 从proc过程中获取调用过程体对应的入口标号
                                      (goto (reg val))))) ;; 转到入口标号去执行
        ((and (not (eq? target 'val)) (not (eq? linkage 'return))) ;; 目标解释器不是 val, 并且连接方式不是 return 
         (let ((proc-return (make-label 'proc-return))) ;; 创建 proc-return 对应的标号
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                (reg proc))
                                        (goto (reg val)) ;; 执行完过程调用后，最后会执行 (goto (reg continue)) 跳转到 proc-return 标号对应处
                                        ,proc-return
                                        (assign ,target (reg val)) ;; 把求值结果放置到 target 寄存器
                                        (goto (label ,linkage)))))) ;; 跳转到整体调用的连接方式（标号或 next）
        ((and (eq? target 'val) (eq? linkage 'return)) ;; 目标解释器是 val, 并且连接方式是 return 
         (make-instruction-sequence '(proc continue) all-regs
                                    '((assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val))))) ;; 不需要设置 continue 寄存器，直接调用  (goto (reg continue)) 此时continue寄存器的值是compile-proc-appl 时的值
        ((and (not (eq? target 'val)) (eq? linkage 'return)) ;; 目标解释器不是 val, 并且连接方式是 return : 非法调用
         (error "return linkage, target not val -- COMPILE"
                target))))

;;;;;;;;;;;;;;;;;;;;
;; 指令序列的组合 ;;
;;;;;;;;;;;;;;;;;;;;

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

;;; 基本顺序序列组合
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1) ;;需要的寄存器是 seq1 所需寄存器加上 seq2 需要而又没有被 seq1 修改的寄存器
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1))) ;; 这里的实现是：seq1的需要寄存器集合，与 （seq2的需要寄存器集合与seq1的修改寄存器集合的差集）之并集 
     (list-union (registers-modified seq1)
                 (registers-modified seq2)) ;; 所修改的寄存器是所有被 seq1 或 seq2 修改的寄存器
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

;;; 集合并集
(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

;;; 集合差集
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

;;; 处理寄存器的压栈入栈操作
;;; regs: 寄存器列表
;;; seq1: 语句1
;;; seq2: 语句2  
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2) ;; 拼接 seq1, seq2 语句
      (let ((first-reg (car regs)))
	(if (and (needs-register? seq2 first-reg) ;; 如果寄存器被 seq2 需要，而又被 seq1 修改，这样的寄存器就需要执行 save 和 restore 操作
		 (modifies-register? seq1 first-reg))
	    (preserving (cdr regs)
			(make-instruction-sequence
			 ;; 调用下一个preserving 需要去掉 first-regs
			 (list-union (list first-reg)
				     (registers-needed seq1)) ;; seq1 需要store寄存器集合：添加first-reg
			 (list-difference (registers-modified seq1) ;; seq1 需要restore寄存器集合：去除first-reg
					  (list first-reg))
			 (append `((save ,first-reg))
				 (statements seq1)
				 `((restore ,first-reg)))) ;; 在 seq1 的语句前后添加 save , store 寄存器的操作
			seq2)
	    (preserving (cdr regs) seq1 seq2)))))

;;; 添加过程体到序列中
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

;;; 并行执行
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
	       (registers-needed seq2))
   (list-union (registers-modified seq1)
	       (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;;;labels (from footnote)
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))
;; end of footnote
(define all-regs '(env proc val argl continue))
