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

;;;;;;;;;;;;;;;;;;;;;;;;
;; set up environment ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list '+ +)
	(list '> >)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (eval exp env)							      ;;
;;   (cond ((self-evaluating? exp) exp) ; (eval '100 env) 			      ;;
;;         ((variable? exp) (lookup-variable-value exp env)) ; (eval x env) 	      ;;
;;         ((quoted? exp) (text-of-quotation exp)) ; (eval '(quote a) env) 	      ;;
;;         ((assignment? exp) (eval-assignment exp env)) ; (eval '(set! a 10) env)    ;;
;;         ((definition? exp) (eval-definition exp env)) ; (eval '(define a 20) env)  ;;
;;         ((if? exp) (eval-if exp env)) ; (eval '(if (> a 3) 100 a) env) 	      ;;
;;         ((lambda? exp) ; (eval '(lambda (x y) (+ x y)) env)  		      ;;
;;          (make-procedure (lambda-parameters exp) 				      ;;
;;                          (lambda-body exp)					      ;;
;;                          env))						      ;;
;;         ((begin? exp) ; (eval '(begin (+ 1 2) (/ 2 1)) env) 			      ;;
;;          (eval-sequence (begin-actions exp) env)) 				      ;;
;;         ((cond? exp) (eval (cond->if exp) env)) 				      ;;
;;         ((application? exp) ; (eval '(add 100 (+ 2 20)) env)			      ;;
;;          (apply (eval (operator exp) env)					      ;;
;;                 (list-of-values (operands exp) env)))			      ;;
;;         (else								      ;;
;;          (error "Unknown expression type -- EVAL" exp))))			      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
	((quoted? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((application? exp) (analyze-application exp))
	(else (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp))) ;; 分析运算符
        (aprocs (map analyze (operands exp)))) ;; 分析运算参数
    (lambda (env)
      (execute-application (fproc env) ;; 执行运算符的执行过程，获得运算符
                           (map (lambda (aproc) (aproc env)) ;; 执行个运算对象的执行过程，得到实参
                                aprocs)))))

;; (define test-environment (setup-environment))
;; ((analyze '(define (add x y) (+ x y))) test-environment)
;; (define add-analyzed (analyze-application '(add 100 (+ 2 20))))
;; (add-analyzed test-environment) ; => 122

;; (define fproc (analyze 'add))
;; (operands '(add 100 (+ 2 20))) 
;; (define aprocs
;;   (map analyze '(100 (+ 2 20))))
;; (define arguments
;;   (map (lambda (aproc) (aproc test-environment))
;;        aprocs))
;; arguments ; =>  (100 22)
;; (execute-application (fproc test-environment) '(100 22)) ; => 122

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (apply procedure arguments)										 ;;
;;   (cond ((primitive-procedure? procedure) 									 ;;
;;          (apply-primitive-procedure procedure arguments)) ; directly call scheme system given apply function  ;;
;;         ((compound-procedure? procedure)									 ;;
;;          (eval-sequence											 ;;
;;            (procedure-body procedure) ; eval procedure body 							 ;;
;;            (extend-environment ; eval arguments and bound them to a new frame 				 ;;
;;              (procedure-parameters procedure)								 ;;
;;              arguments											 ;;
;;              (procedure-environment procedure))))								 ;;
;;         (else												 ;;
;;          (error												 ;;
;;           "Unknown procedure type -- APPLY" procedure))))							 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;; (define test-environment (setup-environment))
;; ((analyze '(define (add x y) (+ x y))) test-environment)
;; (define proc ((analyze 'add) test-environment)) 
;; (execute-application proc '(100 22)) ; => 122

;; (define proc-body (procedure-body proc))  
;; (procedure-parameters proc) ; => (x y)
;; (define test-extended-enviroment (extend-environment '(x y) '(100 22) test-environment))
;; (proc-body test-extended-enviroment) ; => 122 

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

;; ((analyze-self-evaluating 100) '()) ;=> 100 

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

;; ((analyze-quoted '(quote abc)) '()) ;=> abc 

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

;; (define test-extend-dev (extend-environment '(a b) '(300 400) '())) ; => test-extend-dev
;; ((analyze-variable 'a) test-extend-dev) ; => 300   
;; ((analyze-variable 'b) test-extend-dev) ; => 400   

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (eval-if exp env)		       ;;
;;   (if (true? (eval (if-predicate exp) env)) ;;
;;       (eval (if-consequent exp) env)	       ;;
;;       (eval (if-alternative exp) env)))     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

;; (define test-environment (setup-environment))
;; ((analyze-if '(if (null?
;; 		   '(1 2))
;; 		  (+ 3 4) (+ 5 6)))
;;  test-environment) ;=> 11


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

;; (define test-env '()) 
;; ((analyze-lambda '(lambda (x) (+ 1 x))) test-env)
;; => (procedure (x) #[compound-procedure 19] ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (eval-sequence exps env)			  ;;
;;   (cond ((last-exp? exps) (eval (first-exp exps) env)) ;;
;;         (else (eval (first-exp exps) env)		  ;;
;;               (eval-sequence (rest-exps exps) env))))  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env))) 
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
	;; 把前面两个表达式组合起来
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps))) ;; 分析各个子表达式
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;; (define test-environment1 (setup-environment))
;; (define test-environment2 (setup-environment))
;; (define sequence-analyzed
;;   (analyze-sequence '((+ 1 2) true (+ 2 3))))
;; (sequence-analyzed test-environment1) ; => 5
;; (sequence-analyzed test-environment2) ; => 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (eval-definition exp env)			 ;;
;;   (define-variable! (definition-variable exp)	 ;;
;;                     (eval (definition-value exp) env) ;;
;;                     env)				 ;;
;;   'ok)						 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

;; (define test-environment (setup-environment)) 
;; ((analyze-definition '(define a (quote hello))) test-environment) ;=> ok
;; test-environment
;; => (((a false true car cdr cons null? + >)
;;      hello
;;      #f
;;      #t (primitive #[compiled-procedure 20 ("list" #x1) #x1a #x23d73e2])
;;      (primitive #[compiled-procedure 21 ("list" #x2) #x1a #x23d7452])
;;      (primitive #[compiled-procedure 22 ("list" #x3) #x14 #x23d74bc])
;;      (primitive #[compiled-procedure 23 ("list" #x5) #x14 #x23d755c])
;;      (primitive #[arity-dispatched-procedure 24])
;;      (primitive #[arity-dispatched-procedure 25])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (eval-assignment exp env)			    ;;
;;   (set-variable-value! (assignment-variable exp)	    ;;
;;                        (eval (assignment-value exp) env) ;;
;;                        env)				    ;;
;;   'ok)						    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

;; (define test-environment (setup-environment)) 
;; ((analyze-definition '(define a (quote hello))) test-environment) ;=> ok

;; ((analyze-assignment '(set! a (quote world))) test-environment) ;=> ok
;; test-environment
;; => (((a false true car cdr cons null? + >)
;;      world
;;      #f
;;      #t
;;      (primitive #[compiled-procedure 20 ("list" #x1) #x1a #x23d73e2])
;;      (primitive #[compiled-procedure 21 ("list" #x2) #x1a #x23d7452])
;;      (primitive #[compiled-procedure 22 ("list" #x3) #x14 #x23d74bc])
;;      (primitive #[compiled-procedure 23 ("list" #x5) #x14 #x23d755c])
;;      (primitive #[arity-dispatched-procedure 24])
;;      (primitive #[arity-dispatched-procedure 25])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read->eval->print loop ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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

'METACIRCULAR-EVALUATOR-LOADED
