;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METACIRCULAR EVALUATOR 					        ;;
;; 								        ;;
;; This file can be loaded into Scheme as a whole.		        ;;
;; Then you can initialize and start the evaluator by evaluating        ;;
;; the two commented-out lines at the end of the file (setting up the   ;;
;; global environment and starting the driver loop).		        ;;
;; 								        ;;
;; **WARNING: Don't load this file twice (or you'll lose the primitives ;;
;;  interface, due to renamings of apply).			        ;;
;; 								        ;;
;; must precede def of metacircular apply			        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define apply-in-underlying-scheme apply)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; represent the expression  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (amb? exp) (tagged-list? exp 'amb))
;; (amb? '(amb 1 2 3)) ; => #t 
;; (amb? #t) ; => #f 
;; (amb? 1) ; => #f

(define (amb-choices exp) (cdr exp))
;; (amb-choices '(amb 1 2 3)) ; => (1 2 3)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))


(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))

(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structure for procedure and envoirment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (true? x)
  (not (eq? x false))) 

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; amb operations 								  ;;
;; (define (require p)								  ;;
;;   (if (not p) (amb)))							  ;;
;; 										  ;;
;; (define (an-element-of items)						  ;;
;;   (require (not (null? items))) ;; 表为空是计算失败				  ;;
;;   (amb (car items) (an-element-of (cdr items)))) ;; 反之，返回表中任何一个元素 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; just for test ;;
;;;;;;;;;;;;;;;;;;;
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set up environment ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list 'not not)
	(list '+ +)
	(list '> >)
	(list 'list list)
	(list 'prime? prime?)
	;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [moved to start of file] (define apply-in-underlying-scheme apply) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [do later] (define the-global-environment (setup-environment)) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval and apply process  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (eval exp env) ;;
;;   ((analyze exp) env)) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail)) 

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
	((quoted? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((amb? exp) (analyze-amb exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((application? exp) (analyze-application exp))
	(else (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp)))) ; 分析各子表达式的执行过程
    (lambda (env succeed fail) 
      (define (try-next choices) 
        (if (null? choices)
            (fail) ; 没有任何值可以试探的时候，报出失败
            ((car choices) env ; 调用 "第一个可能值" 的执行过程
             succeed ; (car choices) 成功续延：原始的成功续延，实际上就是 amb 执行成功
             (lambda () ; (car choices) 失败续延：尝试下一个可能值
               (try-next (cdr choices))))))
      (try-next cprocs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-application exp)								      ;;
;;   (let ((fproc (analyze (operator exp))) ;; 分析运算符					      ;;
;;         (aprocs (map analyze (operands exp)))) ;; 分析运算参数				      ;;
;;     (lambda (env)										      ;;
;;       (execute-application (fproc env) ;; 执行运算符的执行过程，获得运算符			      ;;
;;                            (map (lambda (aproc) (aproc env)) ;; 执行个运算对象的执行过程，得到实参 ;;
;;                                 aprocs)))))							      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2) ; fproc 的成功续延：依次计算每个实参
               (get-args aprocs
                         env
                         (lambda (args fail3) ; get-args 的成功续延：所有参数都成功计算完毕后，做实际的过程调用
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail)))) 

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env ;  "求值第一个运算参数" 的执行过程
       (lambda (arg fail2) ; (car aprocs) 的成功续延
         (get-args (cdr aprocs) ; 对余下参数进行求值
                   env
                   (lambda (args fail3) ; (get-args (cdr aprocs)) 的成功续延
                     (succeed (cons arg args) ; 用 cons 来收集所有的求值结果，然后把他送给最初调用的成功续延
                              fail3))
                   fail2))
       fail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (execute-application proc args)			 ;;
;;   (cond ((primitive-procedure? proc)				 ;;
;;          (apply-primitive-procedure proc args))		 ;;
;;         ((compound-procedure? proc)				 ;;
;;          ((procedure-body proc)				 ;;
;;           (extend-environment (procedure-parameters proc)	 ;;
;;                               args				 ;;
;;                               (procedure-environment proc)))) ;;
;;         (else						 ;;
;;          (error						 ;;
;;           "Unknown procedure type -- EXECUTE-APPLICATION"	 ;;
;;           proc))))						 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-self-evaluating exp) ;;
;;   (lambda (env) exp))		 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) ;; succeed 过程有 2 个参数，第一个是返回值，第二个是失败续延．而 fail 过程没有参数
    (succeed exp fail))) ;; 直接返回 exp, 把当前的失败续延传递进去

;; ((analyze-self-evaluating 1)
;;  '()
;;  (lambda (value faile) value)
;;  (lambda () 'failed)) ; => 1 

;; ((analyze-self-evaluating "hello")
;;  '()
;;  (lambda (value faile) value)
;;  (lambda () 'failed)) ; => "hello"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-quoted exp)		   ;;
;;   (let ((qval (text-of-quotation exp))) ;;
;;     (lambda (env) qval)))		   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

;; ((analyze-quoted '(quote abc))
;;  '()
;;  (lambda (value faile) value)
;;  (lambda () 'failed)) ; => abc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-variable exp)		     ;;
;;   (lambda (env) (lookup-variable-value exp env))) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;; (define test-extend-dev (extend-environment '(a b) '(300 400) '())) ; => test-extend-dev

;; ((analyze-variable 'a)
;;  test-extend-dev
;;  (lambda (value faile) value)
;;  (lambda () 'failed))  ; => 300   

;; ((analyze-variable 'b)
;;  test-extend-dev
;;  (lambda (value fail) value)
;;  (lambda () 'failed)) ; => 400

;; ((analyze-variable 'c)
;;  test-extend-dev
;;  (lambda (value faile) value)
;;  (lambda () 'failed)) ; => ;Unbound variable c

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-lambda exp)				 ;;
;;   (let ((vars (lambda-parameters exp))		 ;;
;;         (bproc (analyze-sequence (lambda-body exp)))) ;;
;;     (lambda (env) (make-procedure vars bproc env))))	 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;; ((analyze-lambda '(lambda (x) (+ 1 x))) 
;;  '()
;;  (lambda (value faile) value)
;;  (lambda () 'failed)) ; =>  (procedure (x) #[compound-procedure 14] ())

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-if exp)			   ;;
;;   (let ((pproc (analyze (if-predicate exp)))	   ;;
;;         (cproc (analyze (if-consequent exp)))   ;;
;;         (aproc (analyze (if-alternative exp)))) ;;
;;     (lambda (env)				   ;;
;;       (if (true? (pproc env))		   ;;
;;           (cproc env)			   ;;
;;           (aproc env)))))			   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; pproc 过程的成功续延
	     ;; 如果 pproc 过程执行成功，会把计算出的真假值传递给pred-value，以及当前的 fail 传递给 fail2
             (lambda (pred-value fail2)  
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; pproc 过程的失败续延，就是 if 过程的失败续延
             fail))))

;; (define my-succeed (lambda (value fail) value))
;; (define my-fail (lambda () 'failed))
;; (define test-environment (setup-environment))

;; (define if-proc (analyze-if '(if true 100 200)))
;; (if-proc test-environment my-succeed my-fail) ; => 100

;; (define if-pproc (analyze 'true))
;; (define if-cproc (analyze 100))
;; (define if-aproc (analyze 200))

;; (if-pproc test-environment
;; 	  (lambda (pred-value fail2) 
;;                (if (true? pred-value)
;;                    (if-cproc test-environment my-succeed fail2)
;;                    (if-aproc test-environment my-succeed fail2)))
;;              my-fail) ;=> 100

;; ((analyze-variable 'true) test-environment
;; 	  (lambda (pred-value fail2) 
;;                (if (true? pred-value)
;;                    (if-cproc test-environment my-succeed fail2)
;;                    (if-aproc test-environment my-succeed fail2)))
;;              my-fail) ; => 100

;; (define if-pproc-succeed
;;   (lambda (pred-value fail2) 
;;                (if (true? pred-value)
;;                    (if-cproc test-environment my-succeed fail2)
;;                    (if-aproc test-environment my-succeed fail2))))

;; (if-pproc-succeed
;;  (lookup-variable-value 'true test-environment)
;;  my-fail) ; => 100

;; (if (true? (lookup-variable-value 'true test-environment))
;;     (if-cproc test-environment my-succeed my-fail)
;;     (if-aproc test-environment my-succeed my-fail)) ; => 100


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-sequence exps)				   ;;
;;   (define (sequentially proc1 proc2)				   ;;
;;     (lambda (env) (proc1 env) (proc2 env))) 			   ;;
;;   (define (loop first-proc rest-procs)			   ;;
;;     (if (null? rest-procs)					   ;;
;;         first-proc						   ;;
;; 	;; 把前面两个表达式组合起来				   ;;
;;         (loop (sequentially first-proc (car rest-procs))	   ;;
;;               (cdr rest-procs))))				   ;;
;;   (let ((procs (map analyze exps))) ;; 分析各个子表达式	   ;;
;;     (if (null? procs)					   ;;
;;         (error "Empty sequence -- ANALYZE"))			   ;;
;;     (loop (car procs) (cdr procs))))				   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; a 过程的成功续延，如果 a 过程成功执行，计算的结果作为 a-value 传递进下面的成功续延，
         (lambda (a-value fail2) ;; a-value 被舍弃，下面不会用到
           (b env succeed fail2)) ;; 继续执行 b 过程
         ;; a 过程的失败续延，调用传递进来的失败续延
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;; (define my-succeed (lambda (value fail) value))
;; (define my-fail (lambda () 'failed))
;; (define test-environment (setup-environment))

;; (define sequence-proc (analyze-sequence '(100 true "hello")))
;; (sequence-proc test-environment my-succeed my-fail) ; => "hello"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-definition exp)		     ;;
;;   (let ((var (definition-variable exp))	     ;;
;;         (vproc (analyze (definition-value exp)))) ;;
;;     (lambda (env)				     ;;
;;       (define-variable! var (vproc env) env)	     ;;
;;       'ok)))					     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp)))) 
    (lambda (env succeed fail)
      (vproc env ; 当时的环境                       
             (lambda (val fail2)
               (define-variable! var val env) ; 在 vproc 的成功续延里完成在环境中变量的定义
               (succeed 'ok fail2))
             fail))))

;; (define my-succeed (lambda (value fail) value))
;; (define my-fail (lambda () 'failed))
;; (define test-environment (setup-environment))

;; ((analyze-definition '(define a (quote hello))) test-environment my-succeed my-fail) ; => ok
;; test-environment 
;; => (((a false true car cdr cons null? + >)
;;      hello
;;      #f
;;      #t
;;      (primitive #[compiled-procedure 17 ("list" #x1) #x1a #x1fc23e2])
;;      (primitive #[compiled-procedure 18 ("list" #x2) #x1a #x1fc2452])
;;      (primitive #[compiled-procedure 19 ("list" #x3) #x14 #x1fc24bc])
;;      (primitive #[compiled-procedure 20 ("list" #x5) #x14 #x1fc255c])
;;      (primitive #[arity-dispatched-procedure 21])
;;      (primitive #[arity-dispatched-procedure 22])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (analyze-assignment exp)		     ;;
;;   (let ((var (assignment-variable exp))	     ;;
;;         (vproc (analyze (assignment-value exp)))) ;;
;;     (lambda (env)				     ;;
;;       (set-variable-value! var (vproc env) env)   ;;
;;       'ok)))					     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1* 求值表达式的成功续延，先保存变量的原始值，再赋值，赋值完成后，调用传入的 succeed 续延
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env) ; 一旦 succeed 失败，将恢复变量原始值，再调用最初的失败续延
                            (fail2)))))
             fail))))

;; (define my-succeed (lambda (value fail) value))
;; (define my-fail (lambda () 'failed))
;; (define test-environment (setup-environment)) 
;; ((analyze-definition '(define a (quote hello))) test-environment my-succeed my-fail) ; => ok

;; ((analyze-assignment '(set! a (quote world))) test-environment my-succeed my-fail)  ;=> ok
;; test-environment
;; =>  (((a false true car cdr cons null? + >)
;;       world
;;       #f
;;       #t
;;       (primitive #[compiled-procedure 17 ("list" #x1) #x1a #x1fc23e2])
;;       (primitive #[compiled-procedure 18 ("list" #x2) #x1a #x1fc2452])
;;       (primitive #[compiled-procedure 19 ("list" #x3) #x14 #x1fc24bc])
;;       (primitive #[compiled-procedure 20 ("list" #x5) #x14 #x1fc255c])
;;       (primitive #[arity-dispatched-procedure 21])
;;       (primitive #[arity-dispatched-procedure 22])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read->eval->print loop ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define input-prompt ";;; M-Eval input:")  ;;
;; (define output-prompt ";;; M-Eval value:") ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (driver-loop)				   ;;
;;   (prompt-for-input input-prompt)			   ;;
;;   (let ((input (read)))				   ;;
;;     (let ((output (eval input the-global-environment))) ;;
;;       (announce-output output-prompt)		   ;;
;;       (user-print output)))				   ;;
;;   (driver-loop))					   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (driver-loop)
  (define (internal-loop try-again) 
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again) ; 用户输入 "try-again"，调用传入的 try-again 过程
          (try-again)
          (begin ; 开始新的一次求值
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval 的成功续延
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val) ; 打印返回值
                       (internal-loop next-alternative)) ; 把成功求值后得到的失败续延作为 interal-loop 的 try-again 参数
                     ;; ambeval 的失败续延
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input) ; 打印失败信息
                       (driver-loop))))))) ; 重新开始驱动循环
  (internal-loop ; internal-loop 的初始 try-again 过程
   (lambda () ; 显示 "无事可做"，然后重新开始驱动循环
     (newline) 
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following are commented out so as not to be evaluated when ;;
;; the file is loaded.					      ;;
;; (define the-global-environment (setup-environment))	      ;;
;; (driver-loop)					      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'AMB-METACIRCULAR-EVALUATOR-LOADED
