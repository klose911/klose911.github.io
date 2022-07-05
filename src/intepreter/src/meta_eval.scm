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

;; (self-evaluating? '()) ; #f 
;; (self-evaluating? nil) ; #f 
;; (self-evaluating? #t) ; #f 
;; (self-evaluating? 100) ; #t 
;; (self-evaluating? 122.23) ; #t 
;; (self-evaluating? "aaaa") ; #t 
;; (self-evaluating? 'ab) ; #f 

(define (quoted? exp)
  (tagged-list? exp 'quote))

;; (tagged-list? '(define a 1) 'quote) ; #f
;; (tagged-list? '(quote 1 2) 'quote) ; #t

(define (text-of-quotation exp) (cadr exp))

;; (text-of-quotation '(quote abc)) ; abc 

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; (tagged-list? 100 'quote) ; #f
;; (tagged-list? '(define a 1) 'quote) ; #f
;; (tagged-list? '(quote 1 2) 'quote) ; #t

(define (variable? exp) (symbol? exp))
;; (variable? 100) ; #f
;; (variable? "aaaa") ; #f 
;; (variable? 'abc) ; #t 
;; (variable? '_@****) ; #t
;; (variable? '(a)) ; #f

(define (assignment? exp)
  (tagged-list? exp 'set!))
;; (assignment? '(set! a b)) ; #t
;; (assignment? '(quote a)) ; #f

(define (assignment-variable exp) (cadr exp))
;; (assignment-variable '(set! x (+ b 200))) ; x
(define (assignment-value exp) (caddr exp))
;; (assignment-value '(set! x (+ b 200))) ; (+ b 200) 

(define (definition? exp)
  (tagged-list? exp 'define))
;; (definition? '(define zero 0))  ; => #t
;; (definition? '(define add (lambda (x y) (+ x y)))) ; => #t
;; (definition? '(define (sub x y) (- x y))  ; => #t
;; (definition? '(quote define)) ; => #f

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
;; (definition-variable '(define zero 0)) ; => zero
;; (definition-variable '(define add (lambda (x y) (+ x y)))) ; => add
;; (definition-variable '(define (sub x y) (- x y)))) ; => sub

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
;; (definition-value '(define zero 0)) ; => 0

;; (definition-value '(define add (lambda (x y) (+ x y)))) ; => (lambda (x y) (+ x y)) 
;; (cadr '(define add (lambda (x y) (+ x y)))) => add 
;; (caddr '(define add (lambda (x y) (+ x y)))) ; => (lambda (x y) (+ x y)) 

;; (definition-value '(define (sub x y) (- x y))) ; => (lambda (x y) (- x y))
;; (cdadr '(define (sub x y) (- x y))) ; => (x y)
;; (cddr '(define (sub x y) (- x y))) ; => ((- x y))
;; (make-lambda '(x y) '((- x y))) ; =>  (lambda (x y) (-x  y))

(define (lambda? exp) (tagged-list? exp 'lambda))
;; (lambda? '(quote a)) ; #f
;; (lambda? '(lambda (x y) (- x y))) ; #t

(define (lambda-parameters exp) (cadr exp))
;; (lambda-parameters '(lambda (x y) (- x y))) ; => (x y) 

(define (lambda-body exp) (cddr exp))
;; (lambda-body '(lambda (x y) (- x y))) ; => ((- x y)) 

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
;; (make-lambda '(x y) '((- x y))) ; =>  (lambda (x y) (-x  y))


(define (if? exp) (tagged-list? exp 'if))
;; (if? '(quote  a)) ; => #f
;; (if? '(if (> 2 1) 4 )) ; => #t
;; (if? '(if (> a b) 1 2)) ; => #t 

(define (if-predicate exp) (cadr exp))
;; (if-predicate '(if (> 2 1) 4 )) ; => (> 2 1) 
;; (if-predicate '(if (> a b) (+ 1 2) (/ 4 2))) ; => (> a b)

(define (if-consequent exp) (caddr exp))
;; (if-consequent '(if (> 2 1) 4 )) ; => 4 
;; (if-consequent '(if (> a b) (+ 1 2) (/ 4 2))) ; => (+ 1 2)

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
;; (if-alternative '(if (> 2 1) 4 )) ; => false
;; (if-alternative '(if (> a b) (+ 1 2) (/ 4 2))) ; =>  (/ 4 2)

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
;; (make-if '(> a b) '(+ 1 2) '(/ 4 2)) ; =>  (if (> a b) (+ 1 2) (/ 4 2)) 

(define (begin? exp) (tagged-list? exp 'begin))
;; (begin? '(quote a)) ;=> f
;; (begin? '(begin (set! balance (- balance amount)) balance)) ;=> #t

(define (begin-actions exp) (cdr exp))
;; (begin-actions '(begin (set! balance (- balance amount)) balance)) ; => ((set! balance (- balance amount)) balance)

(define (last-exp? seq) (null? (cdr seq)))
;; (last-exp? '((set! balance (- balance amount)) balance)) ; => #f
;; (last-exp? '(balance)) ; => #t
;; (last-exp? '((* 2 5))) ; => #t
;; (last-exp? '(* 2 5)) ; => #f

(define (first-exp seq) (car seq))
;; (first-exp '((set! balance (- balance amount)) balance)) ; => (set! balance (- balance amount)) 
;; (first-exp '((+ 1 2) (/ 4 2) (* 2 5))) ; => (+ 1 2)

(define (rest-exps seq) (cdr seq))
;; (rest-exps '((set! balance (- balance amount)) balance)) ; => (balance)
;; (rest-exps '((/ 4 2) (* 2 5))) ; => ((* 2 5)) 

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
;; (sequence->exp '()) ; => ()
;; (sequence->exp '((+ 1 2))) ; =>  (+ 1 2) 
;; (sequence->exp '((set! balance (- balance amount)) balance)) ; => (begin (set! balance (- balance amount)) balance)
;; (sequence->exp '((+ 1 2) (/ 4 2) (* 2 5))) ; =>  (begin (+ 1 2) (/ 4 2) (* 2 5))

(define (make-begin seq) (cons 'begin seq))
;; (make-begin '((set! balance (- balance amount)) balance)) ; => (begin (set! balance (- balance amount)) balance)
;; (make-begin '((+ 1 2) (/ 4 2) (* 2 5))) ; =>  (begin (+ 1 2) (/ 4 2) (* 2 5)) 

(define (application? exp) (pair? exp))
;; (application? #t) ; => #f 
;; (application? 'ab) ; => #f
;; (application? 1.0) ; => #f 
;; (application? '()) ; => #f 
;; (application? '(a b c)) ; => #t
;; (application? '(deine add_1 (lambda (x) (+ 1 x)))) ; => #t
;; (application? '(/ y 2)) ; => #t
;; (application? '(add (+ 20 x) 40)) ; => #t

(define (operator exp) (car exp))
;; (operator '(/ y 2)) ; => /
;; (operator '(add (+ 20 x) 40)) ; => add

(define (operands exp) (cdr exp))
;; (operands '(/ y 2)) ; => (y 2)
;; (operands '(add (+ 20 x) 40)) ; =>  ((+ 20 x) 40)

(define (no-operands? ops) (null? ops))
;; (no-operands? '())  ; => #t
;; (no-operands? '(y 2))  ; => #f
;; (no-operands? '((+ 20 x) 40))  ; => #f

(define (first-operand ops) (car ops))
;; (first-operand '(y 2)) ; => y
;; (first-operand '((+ 20 x) 40)) ; => (+ 20 x)

(define (rest-operands ops) (cdr ops))
;; (rest-operands '(x)) ; => ()
;; (rest-operands '(y 2)) ; => (2)
;; (rest-operands '(y (+ 20 x) 40)) ; => ((+ 20 x) 40)

(define (cond? exp) (tagged-list? exp 'cond))
;; (cond? '(cond
;; 	 ((null? seq) seq)
;; 	 ((last-exp? seq) (first-exp seq))
;; 	 (else (make-begin seq)))) ; => #t

(define (cond-clauses exp) (cdr exp))
;; (cond-clauses '(cond
;; 	 ((null? seq) seq)
;; 	 ((last-exp? seq) (first-exp seq))
;; 	 (else (make-begin seq))))
;; => (((null? seq) seq)
;;     ((last-exp? seq) (first-exp seq))
;;     (else (make-begin seq)))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
;; (cond-else-clause? '((null? seq) seq)) ; => #f
;; (cond-else-clause? '(else (make-begin seq))) ; => #t 

(define (cond-predicate clause) (car clause))
;; (cond-predicate '((null? seq) seq)) ; => (null? seq) 
;; (cond-predicate '((last-exp? seq) (first-exp seq))) ; => (last-exp? seq) 
;; (cond-predicate '(else (make-begin seq))) ; => else

(define (cond-actions clause) (cdr clause))
;; (cond-actions '((null? seq) seq)) ; => (seq) 
;; (cond-actions '((last-exp? seq) (first-exp seq))) ; => ((first-exp seq))
;; (cond-actions '(else (make-begin seq))) ; => ((make-begin seq))

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

;; (expand-clauses '()) ; => false

;; (expand-clauses '(((null? seq) seq)
;; 		     ((last-exp? seq) (first-exp seq))
;; 		     (else (make-begin seq))))
;; => (if
;;     (null? seq) seq
;;     (if (last-exp? seq)
;; 	(first-exp seq)
;; 	(make-begin seq)))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
;; (cond-clauses '(cond ((null? seq) seq)
;; 	             ((last-exp? seq) (first-exp seq))
;; 	             (else (make-begin seq))))
;; => (((null? seq) seq)
;;     ((last-exp? seq) (first-exp seq))
;;     (else (make-begin seq))

;; (cond->if '(cond ((null? seq) seq)
;; 	          ((last-exp? seq) (first-exp seq))
;; 	          (else (make-begin seq))))
;; => (if
;;     (null? seq) seq
;;     (if (last-exp? seq)
;; 	(first-exp seq)
;; 	(make-begin seq)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structure for procedure and envoirment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (true? x)
  (not (eq? x false))) 
;; (true? 100) ; => #t 
;; (true? "hello") ; => #t 
;; (true? 'a) ; => #t
;; (true? '(a b c)) ; => #t 
;; (true? '()) ;=> #t

;; (true? false) ; => #f 
;; (true? (not true)) ; => #f 

(define (false? x)
  (eq? x false))
;; (false? 100) ; => #f
;; (false? "hello") ; => #f
;; (false? 'a) ; => #f
;; (false? '(a b c)) ; => #f
;; (false? '()) ;=> #f

;; (false? false) ; => #t 
;; (false? (not true)) ; => #t 

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
;; (make-procedure '(x y) '(+ x y) '()) ; =>  (procedure (x y) (+ x y) ())
;; (make-procedure '() '(/ 20 5) '()) ; => (procedure () (/ 20 5) ()) 

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
;; (compound-procedure? '(procedure (x y) (+ x y) ())) ; =>#t
;; (compound-procedure? '(procedure () (/ 20 5) ())) ; => #t 
;; (compound-procedure? '(primitive () (/ 20 5) ())) ; => #f 

(define (procedure-parameters p) (cadr p))
;; (procedure-parameters '(procedure (x y) (+ x y) ())) ; => (x y)
;; (procedure-parameters '(procedure () (/ 20 5) ())) ; => ()

(define (procedure-body p) (caddr p))
;; (procedure-body '(procedure (x y) (+ x y) ())) ; => (+ x y)
;; (procedure-body '(procedure () (/ 20 5) ())) ; =>  (/ 20 5)

(define (procedure-environment p) (cadddr p))
;; (procedure-environment '(procedure (x y) (+ x y) (((a b) 300 400)))) ; => (((a b) 300 400)) 
;; (cadddr '(procedure (x y) (+ x y) (((a b) 300 400)))) ; =>  (((a b) 300 400))

;; (procedure-environment '(procedure (x y) (+ x y) (((x y z) "hello" "world"  (procedure (u v) (+ u v)))
;; 						  ((a b) 300 400)))) ; => (((x y z) "hello" "world" (procedure (u v) (+ u v))) ((a b) 300 400))

				   
(define (enclosing-environment env) (cdr env))
;; (enclosing-environment '(((a b) 300 400))) ; => () 
;; (enclosing-environment '(((x y z) "hello" "world"  (procedure (u v) (+ u v)))
;; 			 ((a b) 300 400)))  ; =>  (((a b) 300 400)) 
;; (enclosing-environment '(((add) (procedure (u v) (+ u v))) 
;; 			 ((x y) "hello" "world")
;; 			 ((a b) 300 400))) ; => (((x y) "hello" "world") ((a b) 300 400))

(define (first-frame env) (car env))
;; (first-frame '(((a b) 300 400))) ; => ((a b) 300 400) 
;; (first-frame '(((x y z) "hello" "world"  (procedure (u v) (+ u v)))
;; 			 ((a b) 300 400)))  ; => ((x y z) "hello" "world" (procedure (u v) (+ u v)))  
;; (first-frame '(((add) (procedure (u v) (+ u v))) 
;; 			 ((x y) "hello" "world")
;; 			 ((a b) 300 400))) ; => ((add) (procedure (u v) (+ u v))) 

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
;; (make-frame '(x) '(100)) ; => ((x) 100) 
;; (make-frame '(x y) '(100 200))  ; => ((x y) 100 200) 
;; (make-frame '(x y add) '(100 200 (lambda (x y) (+ x y)))) ; => ((x y add) 100 200 (lambda (x y) (+ x y))) 

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
;; (define test-frame (make-frame '(x y add) '(100 200 (lambda (x y) (+ x y))))) ; => test-frame
;; (frame-variables test-frame) ; => (x y add) 
;; (frame-values test-frame) ; =>  (100 200 (lambda (x y) (+ x y)))
;; (add-binding-to-frame! 'a '(300 400) test-frame) ; => ((a x y add) (300 400) 100 200 (lambda (x y) (+ x y)) 

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; (extend-environment '(a) '(300 500) '()) ; => Too many arguments supplied (a) (300 500) 
;; (extend-environment '(a b) '(300) '()) ; => ;Too few arguments supplied (a b) (300)

;; (define test-extend-dev (extend-environment '(a b) '(300 400) '())) ; => test-extend-dev
;; test-extend-dev ; => (((a b) 300 400))

;; (define test-extend-dev2 (extend-environment '(x y add) '("hello" "world" (procedure (u v) (+ u v))) test-extend-dev)) ; => test-extend-dev2
;; test-extend-dev2
;; => (
;;     ((x y z) "hello" "world"  (procedure (u v) (+ u v)))
;;     ((a b) 300 400)
;;     )

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
;; (define test-extend-dev (extend-environment '(a b) '(300 400) '())) ; => test-extend-dev
;; (lookup-variable-value 'b test-extend-dev) ; => 400 
;; (lookup-variable-value 'c test-extend-dev) ; => ;Unbound variable c

;; (define test-extend-dev2 (extend-environment '(b c) '("hello" "world") test-extend-dev))
;; test-extend-dev2 ; => (((b c) "hello" "world") ((a b) 300 400))
;; (lookup-variable-value 'a test-extend-dev2) ; => 300 
;; (lookup-variable-value 'b test-extend-dev2) ; => "hello"
;; (lookup-variable-value 'c test-extend-dev2) ; => "world"

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

;; (define test-extend-dev (extend-environment '(a b) '(300 400) '())) ; => test-extend-dev
;; (set-variable-value! 'c 100 '()) ; => ;Unbound variable -- SET! a
;; (set-variable-value! 'a 100 test-extend-dev) ; => Unspecified return value
;; test-extend-dev ; => (((a b) 100 400))

;; (define test-extend-dev2 (extend-environment '(b c) '("hello" "world") test-extend-dev)) 
;; test-extend-dev2 ; => (((b c) "hello" "world") ((a b) 100 400))
;; (set-variable-value! 'a 300 test-extend-dev2)
;; test-extend-dev2 ; => (((b c) "hello" "world") ((a b) 300 400))
;; test-extend-dev ; => (((a b) 300 400))
;; (set-variable-value! 'b "new" test-extend-dev2) 
;; test-extend-dev2 ; => (((b c) "new" "world") ((a b) 300 400))
;; (set-variable-value! 'c "value" test-extend-dev2) 
;; test-extend-dev2 ; =>  (((b c) "new" "value") ((a b) 300 400))

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

;; (define test-extend-dev (extend-environment '(a b) '(300 400) '()))
;; (define-variable! 'c 500 test-extend-dev)
;; test-extend-dev ; => (((c a b) 500 300 400))
;; (define-variable! 'a 200 test-extend-dev)
;; test-extend-dev ; => (((c a b) 500 200 400))

;; (define test-extend-dev2 (extend-environment '(b c) '("hello" "world") test-extend-dev)) 
;; test-extend-dev2 ; => (((b c) "hello" "world") ((c a b) 500 200 400))
;; (define-variable! 'a "my" test-extend-dev2) 
;; test-extend-dev2 ; => (((a b c) "my" "hello" "world") ((c a b) 500 200 400))
;; (define-variable! 'b "new" test-extend-dev2) 
;; test-extend-dev2 ; =>  (((a b c) "my" "new" "world") ((c a b) 500 200 400))
;; (define-variable! 'd "test" test-extend-dev2) 
;; test-extend-dev2 ; => (((d a b c) "test" "my" "new" "world") ((c a b) 500 200 400))


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
;; primitive-procedures 
;; => ((car #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2])
;;     (cdr #[compiled-procedure 95 ("list" #x2) #x1a #x1921452])
;;     (cons #[compiled-procedure 96 ("list" #x3) #x14 #x19214bc])
;;     (null? #[compiled-procedure 97 ("list" #x5) #x14 #x192155c])) 

(define (primitive-procedure-names)
  (map car
       primitive-procedures))
;; (primitive-procedure-names) ; =>  (car cdr cons null? +)

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
;; (primitive-procedure-objects)
;; => ((primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2])
;;     (primitive #[compiled-procedure 95 ("list" #x2) #x1a #x1921452])
;;     (primitive #[compiled-procedure 96 ("list" #x3) #x14 #x19214bc])
;;     (primitive #[compiled-procedure 97 ("list" #x5) #x14 #x192155c])
;;     (primitive #[arity-dispatched-procedure 103]))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
;; (primitive-procedure? '(primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2])) ; => #t 

(define (primitive-implementation proc) (cadr proc))
;; (primitive-implementation '(primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2]))
;; => #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2] 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [moved to start of file] (define apply-in-underlying-scheme apply) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
;; (apply-in-underlying-scheme car '((1 2 3)) ; => 1
;; car ; =>  #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2] 
;; (primitive-implementation '(primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2])) ; => #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2]
;; (apply-primitive-procedure '(primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2]) '((1 2 3))) ; => 1 


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

;; (define test-environment (setup-environment))
;; (((false true car cdr cons null?)
;;   #f
;;   #t
;;   (primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2])
;;   (primitive #[compiled-procedure 95 ("list" #x2) #x1a #x1921452])
;;   (primitive #[compiled-procedure 96 ("list" #x3) #x14 #x19214bc])
;;   (primitive #[compiled-procedure 97 ("list" #x5) #x14 #x192155c])
;;   (primitive #[arity-dispatched-procedure 103])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval and apply process  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp) ; (eval '100 env) 
        ((variable? exp) (lookup-variable-value exp env)) ; (eval x env) 
        ((quoted? exp) (text-of-quotation exp)) ; (eval '(quote a) env) 
        ((assignment? exp) (eval-assignment exp env)) ; (eval '(set! a 10) env)
        ((definition? exp) (eval-definition exp env)) ; (eval '(define a 20) env) 
        ((if? exp) (eval-if exp env)) ; (eval '(if (> a 3) 100 a) env) 
        ((lambda? exp) ; (eval '(lambda (x y) (+ x y)) env)  
         (make-procedure (lambda-parameters exp) 
                         (lambda-body exp)
                         env))
        ((begin? exp) ; (eval '(begin (+ 1 2) (/ 2 1)) env) 
         (eval-sequence (begin-actions exp) env)) 
        ((cond? exp) (eval (cond->if exp) env)) 
        ((application? exp) ; (eval '(add 100 (+ 2 20)) env)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure) 
         (apply-primitive-procedure procedure arguments)) ; directly call scheme system given apply function 
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure) ; eval procedure body 
           (extend-environment ; eval arguments and bound them to a new frame 
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;; (define test-environment (setup-environment))
;; test-environment
;; => (((false true car cdr cons null? +)
;;      #f
;;      #t
;;      (primitive #[compiled-procedure 18 ("list" #x1) #x1a #x12ff3e2])
;;      (primitive #[compiled-procedure 19 ("list" #x2) #x1a #x12ff452])
;;      (primitive #[compiled-procedure 20 ("list" #x3) #x14 #x12ff4bc])
;;      (primitive #[compiled-procedure 21 ("list" #x5) #x14 #x12ff55c])
;;      (primitive #[arity-dispatched-procedure 22])))

;; apply primitive-procedure
;; (apply (eval 'car test-environment) '((1 2 3))) ; => 1
;; (eval 'car test-environment) ; => (primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2]) 
;; (apply '(primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2]) '((1 2 3))) ; => 1 
;; (apply-primitive-procedure '(primitive #[compiled-procedure 94 ("list" #x1) #x1a #x19213e2]) '((1 2 3))) ; => 1

;; apply compound-procedure 
;; (define add-procedure (make-procedure '(x y) '(+ x y) test-environment))
;; add-procedure
;; => (procedure (x y)
;; 	      (+ x y)
;; 	      (((false true car cdr cons null? +)
;; 		#f
;; 		#t
;; 	        (primitive #[compiled-procedure 18 ("list" #x1) #x1a #x12ff3e2])
;; 	        (primitive #[compiled-procedure 19 ("list" #x2) #x1a #x12ff452])
;; 	        (primitive #[compiled-procedure 20 ("list" #x3) #x14 #x12ff4bc])
;; 	        (primitive #[compiled-procedure 21 ("list" #x5) #x14 #x12ff55c])
;; 	        (primitive #[arity-dispatched-procedure 22]))))

;; (apply add-procedure '(12 24)) ; => 36
;; (procedure-body add-procedure) ; => (+ x y)
;; (procedure-parameters add-procedure) ; => (x y)
;; (procedure-environment add-procedure) ; => test-environment 

;; (define add-extended-envoriment
;;   (extend-environment
;;              '(x y) 
;;              '(12 24) 
;;              test-environment))   
;; add-extended-envoriment
;; => ( ((x y)
;;       12
;;       24) ;  bounded variables 
;;      ; test-environment 
;;      ((false true car cdr cons null? +)
;;       #f
;;       #t
;;       (primitive #[compiled-procedure 18 ("list" #x1) #x1a #x12ff3e2])
;;       (primitive #[compiled-procedure 19 ("list" #x2) #x1a #x12ff452])
;;       (primitive #[compiled-procedure 20 ("list" #x3) #x14 #x12ff4bc])
;;       (primitive #[compiled-procedure 21 ("list" #x5) #x14 #x12ff55c])
;;       (primitive #[arity-dispatched-procedure 22])))

;; (eval '(+ x y)  add-extended-envoriment) ; => 36
;; (apply (eval (operands '(+x y)) add-extended-envoriment) (list-of-values (operands '(+ x y)) add-extended-envoriment)) 
;; (eval (operator '(+ x y)) add-extended-envoriment) ; =>  (primitive #[arity-dispatched-procedure 22])
;; (list-of-values (operands '(+ x y)) add-extended-envoriment) ; => (12 24) 
;; (apply '(primitive #[arity-dispatched-procedure 22]) '(12 24))  ; => 36  


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
;; (list-of-values '(1 2) '()) ; => (1 2)

;; (define test-environment (setup-environment))
;; (first-operand '((car '(1 2)) (cdr '(3 4)))) ; => (car (quote (1 2)))
;; (eval '(car '(1 2)) test-environment) ; => 1
;; (rest-operands '((car '(1 2)) (cdr '(3 4)))) ; => ((cdr (quote (3 4))))
;; (list-of-values '((cdr (quote (3 4)))) test-environment) ; => ((4))
;; (cons 1 '((4))) ; => (1 (4)) 
;; (list-of-values '((car '(1 2)) (cdr '(3 4))) test-environment) ; => (1 (4)) 

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; (define test-environment (setup-environment))
;; (eval '(if (null? '(1 2)) (+ 3 4) (+ 5 6)) test-environment) ;=> 11
;; (if-predicate '(if (null? '(1 2)) (+ 3 4) (+ 5 6))) ; => (null? (quote (1 2))) 
;; (eval '(null? '(1 2)) test-environment) ; => #f
;; (if-alternative '(if (null? '(1 2)) (+ 3 4) (+ 5 6)))  ; => (+ 5 6)
;; (eval '(+ 5 6) test-environment) ; => 11 

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; (define test-environment (setup-environment))
;; (eval-sequence '((+ 1 2) true (+ 2 3)) test-environment) ; =>5 
;; (last-exp? '((+ 1 2) true (+ 2 3))) ; => #f
;; (first-exp '((+ 1 2) true (+ 2 3))) ; => (+ 1 2)
;; (eval '(+ 1 2) test-environment) ; => 5
;; (rest-exps '((+ 1 2) true (+ 2 3))) ; => (true (+ 2 3))

;; (eval-sequence '(true (+ 2 3)) test-environment) ; =>5
;; (last-exp? '(true (+ 2 3))) ; => #f
;; (first-exp '(true (+ 2 3))) ; => true
;; (eval 'true test-environment) ; => #t
;; (rest-exps '(true (+ 2 3))) ; => ((+ 2 3))

;; (eval-sequence '((+ 2 3)) test-environment) ; =>5
;; (last-exp? '((+ 2 3))) ; => #t
;; (first-exp '((+ 2 3))) ; => (+ 2 3)
;; (eval '(+ 2 3) test-environment) ; => 5

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; (define test-environment (setup-environment))
;; (eval-definition '(define a (+ 100 200)) test-environment) ; => ok
;; (definition-variable '(define a (+ 100 200))) ; => a
;; (definition-value '(define a (+ 100 200))) ; => (+ 100 200)
;; (eval '(+ 100 200) test-environment) ; => 300 
;; (define-variable! 'a 300 test-environment) ; => 
;; test-environment
;; => (((a false true car cdr cons null? +)
;;      300
;;      #f
;;      #t
;;      (primitive #[compiled-procedure 26 ("list" #x1) #x1a #x1bf9052])
;;      (primitive #[compiled-procedure 27 ("list" #x2) #x1a #x1bf90c2])
;;      (primitive #[compiled-procedure 28 ("list" #x3) #x14 #x1bf912c])
;;      (primitive #[compiled-procedure 29 ("list" #x5) #x14 #x1bf91cc])
;;      (primitive #[arity-dispatched-procedure 30])))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;; (define test-environment (setup-environment))
;; (eval-definition '(define a (+ 100 200)) test-environment)
;; test-environment
;; => (((a false true car cdr cons null? +)
;;      300
;;      #f
;;      #t
;;      (primitive #[compiled-procedure 26 ("list" #x1) #x1a #x1bf9052])
;;      (primitive #[compiled-procedure 27 ("list" #x2) #x1a #x1bf90c2])
;;      (primitive #[compiled-procedure 28 ("list" #x3) #x14 #x1bf912c])
;;      (primitive #[compiled-procedure 29 ("list" #x5) #x14 #x1bf91cc])
;;      (primitive #[arity-dispatched-procedure 30])))

;; (eval-assignment '(set! a (+ 200 300)) test-environment) ; => ok 
;; (assignment-variable '(set! a (+ 200 300))) ; => a
;; (assignment-value '(set! a (+ 200 300))) ; =>  (+ 200 300)
;; (eval '(+ 200 300) test-environment) ; => 500
;; (set-variable-value! 'a 500 test-environment)
;; test-environment
;; => (((a false true car cdr cons null? +)
;;      500
;;      #f
;;      #t
;;      (primitive #[compiled-procedure 26 ("list" #x1) #x1a #x1bf9052])
;;      (primitive #[compiled-procedure 27 ("list" #x2) #x1a #x1bf90c2])
;;      (primitive #[compiled-procedure 28 ("list" #x3) #x14 #x1bf912c])
;;      (primitive #[compiled-procedure 29 ("list" #x5) #x14 #x1bf91cc])
;;      (primitive #[arity-dispatched-procedure 30])))

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

;; (define (inc1 x0) 
;;   (begin (define x x0)
;; 	 (lambda ()
;; 	   (begin (set! x (+ x 1)) 
;; 		  x))))

;; (define inc1-init-10 (inc1 10)) 
;; (inc1-init-10) 
;; (inc1-init-10)

