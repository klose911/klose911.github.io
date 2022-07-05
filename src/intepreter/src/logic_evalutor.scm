;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOGIC METACIRCULAR EVALUATOR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "stream.scm")
(load "operation_table.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query Syntax Procedure  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

;; (type '(assert! (job (Bitdiddle Ben) (computer wizard)))) ; assert!
;; (type '(assert! (rule (wheel ?person)
;; 		      (and (supervisor ?middle-manager ?person)
;; 			   (supervisor ?x ?middle-manager))))) ; assert! 
;; (type '(and (job ?x (computer programmer))
;; 	    (supervisor ?x ?y))) ; and 
;; (type 1) ;Unknown expression TYPE 1

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

;; (contents '(assert! (job (Bitdiddle Ben) (computer wizard)))) ;  ((job (bitdiddle ben) (computer wizard)))
;; (contents '(assert! (rule (wheel ?person)
;; 			  (and (supervisor ?middle-manager ?person)
;; 			       (supervisor ?x ?middle-manager)))))
					; ((rule (wheel ?person) (and (supervisor ?middle-manager ?person) (supervisor ?x ?middle-manager)))) 
;; (contents '(and (job ?x (computer programmer))
;; 	    (supervisor ?x ?y))) ; ((job ?x (computer programmer)) (supervisor ?x ?y)) 
;; (contents 1) ;Unknown expression CONTENTS 1

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

;; (assertion-to-be-added? '(assert! (job (Bitdiddle Ben) (computer wizard)))) ; #t
;; (assertion-to-be-added? '(assert! (rule (wheel ?person)
;; 					(and (supervisor ?middle-manager ?person)
;; 					     (supervisor ?x ?middle-manager))))) ; #t
;; (assertion-to-be-added? '(and (job ?x (computer programmer))
;; 			      (supervisor ?x ?y))) ;#f

(define (add-assertion-body exp)
  (car (contents exp)))

;; (add-assertion-body '(assert! (job (Bitdiddle Ben) (computer wizard))))
;; => (job (bitdiddle ben) (computer wizard)) 
;; (add-assertion-body '(assert! (rule (wheel ?person)
;; 					(and (supervisor ?middle-manager ?person)
;; 					     (supervisor ?x ?middle-manager)))))
;; =>(rule (wheel ?person) (and (supervisor ?middle-manager ?person) (supervisor ?x ?middle-manager)))

;;; and 
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

;; (rest-conjuncts '((supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))
;; ((supervisor (? x) (? middle-manager)))
;; (first-conjunct '((supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))
;; (supervisor (? middle-manager) (? person))
;; (empty-conjunction? ' ((supervisor (? x) (? middle-manager)))) ; #f

;;; or
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

;; (rest-disjuncts '((supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))
;; => ((supervisor (? x) (? middle-manager)))
;; (first-disjunct '((supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))
;; => (supervisor (? middle-manager) (? person))
;; (empty-disjunction? ' ((supervisor (? x) (? middle-manager)))) ; #f

;;; not 
(define (negated-query exps) (car exps))

;;(negated-query '((supervisor (? x) (? middle-manager))))
;; => (supervisor (? x) (? middle-manager)) 

;;; lisp-value
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

;; (predicate '(> (? amount) 30000)) ;; > 
;; (args '(> (? amount) 30000)) ;; ((? amount) 30000) 

;;; rule syntax 
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (var? exp)
  (tagged-list? exp '?))

;; (var? '(? x)) ; #t
;; (var? '(rule b)) ;#f 

(define (constant-symbol? exp) (symbol? exp))

;; (constant-symbol? 1) ;#f
;; (constant-symbol? "hello wold") ;#f
;; (constant-symbol? 'rule) ;#t
;; (constant-symbol? '?x) ;#t
;; (constant-symbol? '(a b c)) ;#f

(define (rule? statement)
  (tagged-list? statement 'rule))

;; (rule? '(job (Bitdiddle Ben) (computer wizard))) ; #f 
;; (rule? '(rule (wheel ?person)
;;       (and (supervisor ?middle-manager ?person)
;; 	   (supervisor ?x ?middle-manager)))) ; #t

(define (conclusion rule) (cadr rule)) ; 结论

;; (conclusion  '(rule (wheel ?person)
;;       (and (supervisor ?middle-manager ?person)
;; 	   (supervisor ?x ?middle-manager)))) ; (wheel ?person)

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule))) ; 规则的体

;; (rule-body  '(rule (wheel ?person)
;;       (and (supervisor ?middle-manager ?person)
;; 	   (supervisor ?x ?middle-manager)))) ; (and (supervisor ?middle-manager ?person) (supervisor ?x ?middle-manager))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

;; (map-over-symbols expand-question-mark '(rule (wheel ?person)
;; 					      (and (supervisor ?middle-manager ?person)
;; 						   (supervisor ?x ?middle-manager))))
;; =>  (rule (wheel (? person)) (and (supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol))) ; 取得 symbol 的名字字符串
    (if (string=? (substring chars 0 1) "?") ; 名字的第一个字符是否 '?
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars)))) ; 取得 symbol 的名字除去 '? 后的字符串
        symbol)))

;; (expand-question-mark '?wheel) ; => (? wheel)
;; (expand-question-mark 'wheel) ; => wheel

;; (query-syntax-process '(assert! (job (Bitdiddle Ben) (computer wizard))))
;; => (assert! (job (bitdiddle ben) (computer wizard)))
;; (query-syntax-process '(assert! (rule (wheel ?person)
;; 				      (and (supervisor ?middle-manager ?person)
;; 					   (supervisor ?x ?middle-manager))))) 
;; => (assert! (rule (wheel (? person)) (and (supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager)))))

;; (query-syntax-process '(assert! (rule (append-to-form () ?y ?y))))
;; => (assert! (rule (append-to-form () (? y) (? y))))

;; (query-syntax-process '(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
;;				      (append-to-form ?v ?y ?z))))
;; => (assert! (rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z)))) 

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(make-new-variable '(? wheel) rule-counter) ; (? 0 wheel)

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?" 
		  (if (number? (cadr variable)) ; 换名变量的特点是表里的第二个元素是数
		      (string-append (symbol->string (caddr variable))
				     "-"
				     (number->string (cadr variable)))
		      (symbol->string (cadr variable))))))

;; (contract-question-mark  '(? 0 wheel)) ; ?wheel-0
;; (cadr '(? 0 wheel)) ; 0 
;; (caddr '(? 0 wheel)) ; wheel
;; (string-append (symbol->string 'wheel)
;; 	       "-"
;; 	       (number->string 0)) ; "wheel-0"
;; (string->symbol
;;  (string-append "?" "wheel-0")) ; ?wheel-0

;; (contract-question-mark  '(? wheel)) ; ?wheel

;;;;;;;;;;;;;;;;;;;;;;;
;; Mantain Database ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (use-index? pat)
  (constant-symbol? (car pat)))

;; (use-index? '(? wheel person)) ; #t
;; (use-index? '(1 2)) ; #f

(define (indexable? pat)
  (or (constant-symbol? (car pat)) ; 模式的 car 是常量符号
      (var? (car pat)))) ; 模式（结论）的 car 是模式变量

;; (indexable? ' (job (Bitdiddle Ben) (computer wizard))) ; #t
;; (indexable? '(wheel ? person)) ; #t
;; (indexable? '(wheel person)) ; #t
;; (indexable? '(1 2 3)) ;#f  

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key))) 

;; (index-key-of '(job (Bitdiddle Ben) (computer wizard))) ; job
;; (index-key-of '(? wheel person)) ; ?

;;; assertions
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern) ; pattern 的 car 是常量
      (get-indexed-assertions pattern) ; 到特定的流里去检索
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream)))))) 

;; (get-stream 'job 'assertion-stream) ; 
;; (store-assertion-in-index '(job (Bitdiddle Ben) (computer wizard)))
;; =>  ((job (bitdiddle ben) (computer wizard)) . #[promise 39]) 
;; (indexable? '(job (Bitdiddle Ben) (computer wizard))) ; #t
;; (index-key-of '(job (Bitdiddle Ben) (computer wizard))) ; job

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

;; (add-assertion! '(job (Bitdiddle Ben) (computer wizard)))
;; => ((job (bitdiddle ben) (computer wizard)) . #[promise 15])
;; (display-stream THE-ASSERTIONS)
;; => (job (bitdiddle ben) (computer wizard))
;; (display-stream (get 'job 'assertion-stream))
;; => (job (bitdiddle ben) (computer wizard)) 

;; (add-assertion! '(job (Klose Wu) (computer noob))) 
;; ((job (klose wu) (computer noob)) . #[promise 19])
;; (display-stream THE-ASSERTIONS) 
;; => (job (klose wu) (computer noob))
;; (job (bitdiddle ben) (computer wizard))
;; (display-stream (get 'job 'assertion-stream))
;; => (job (klose wu) (computer noob))
;; (job (bitdiddle ben) (computer wizard))

;; (add-assertion! '(boss (fabian li)  (mule kai)))
;; (display-stream THE-ASSERTIONS)
;; => (boss (fabian li) (mule kai))
;; (job (klose wu) (computer noob))
;; (job (bitdiddle ben) (computer wizard))
;; (display-stream (get 'boss 'assertion-stream))
;; (boss (fabian li) (mule kai))

;;; rules
(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream))) ; 所有结论的 car 是变量的规则存入 ?索引 的流

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

;; (add-rule! '(rule (wheel (? person)) (and (supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager)))))
;; (display-stream (get-stream 'wheel 'rule-stream))
;; => (rule (wheel (? person)) (and (supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))
;; (display-stream THE-RULES)
;; => (rule (wheel (? person)) (and (supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))

;; (display-stream (get-indexed-rules '(wheel (? person))))
;; => (rule (wheel (? person)) (and (supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager)))) 

;; (add-rule! '(rule (append-to-form () (? y) (? y))))
;; (add-rule! '(rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z))))
;; (display-stream THE-RULES)
;; => (rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z)))
;;    (rule (append-to-form () (? y) (? y)))
;;    (rule (wheel (? person)) (and (supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))
;; (display-stream (get-stream 'append-to-form 'rule-stream)) 
;; => (rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z)))
;;    (rule (append-to-form () (? y) (? y)))
;; (display-stream (get-indexed-rules '(append-to-form ((? u) ? v) (? y) ((? u) ? z)))) 
;; => (rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z)))
;;    (rule (append-to-form () (? y) (? y)))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

;; (conclusion '(rule (wheel (? person)) (and (supervisor (? middle-manager) (? person)) (supervisor (? x) (? middle-manager))))) ;; (wheel (? person))

;;; add rule or assertion 
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

;;;;;;;;;;;;;;;;;;;;;;;
;; binding and frame ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;; (define test-frame '())
;; (set! test-frame (extend 'x 100 test-frame)) 
;; (binding-in-frame 'x test-frame) ;; (x . 100)
;; (set! test-frame (extend 'x 200 test-frame)) 
;; (binding-in-frame 'x test-frame) ;; (x . 200)
;; (set! test-frame (extend 'y 300 test-frame))
;; (binding-in-frame 'x test-frame) ;; (x . 200)
;; (binding-variable (binding-in-frame 'x test-frame)) ; x
;; (binding-value (binding-in-frame 'x test-frame)) ; 200
;; (binding-in-frame 'y test-frame) ;; (y . 300)
;; (binding-variable (binding-in-frame 'y test-frame)) ; y
;; (binding-value (binding-in-frame 'y test-frame)) ; 300

;;;;;;;;;;;
;; Query ;;
;;;;;;;;;;;
;;; simple query 
(define (simple-query query-pattern frame-stream)
  (stream-flatmap ; 把处理各框架得到的流合并为一个流 
   (lambda (frame) 
     (stream-append-delayed ; 组合两个流
      (find-assertions query-pattern frame) ; 找数据库里的匹配断言，生成扩充框架的流 
      (delay (apply-rules query-pattern frame)))) ; 应用可应用的规则，生成扩充框架的流 
   frame-stream))

;; (define init-frame-stream (singleton-stream '())) 
;; (display-stream (simple-query '(append-to-form (a b) (c d) (? z)) init-frame-stream)) 
;; (find-assertions '(append-to-form (a b) (c d) (? z)) init-frame-stream) ; () 
;; (apply-rules '(append-to-form (a b) (c d) (? z)) init-frame-stream)

;;; and queries
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)

;;; or queries
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

;;; not filters 
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands) ; 逆条件查询
			      (singleton-stream frame))) ; 框架流中的某个框架是否满足 逆查询
         (singleton-stream frame) ; 满足的话，加入结果流
         the-empty-stream)) ; 不满足的话，过滤掉
   frame-stream))

(put 'not 'qeval negate) 

;;; lisp-value filters
(define (lisp-value call frame-stream) ; call 被应用的谓词
  (stream-flatmap
   (lambda (frame)
     (if (execute ; 处理实例化后的谓词，类似于 eval，但不求值谓词的参数（因它们已经是值）
          (instantiate ; instantiate 用 frame 实例化 call 里的变量，得到所需的谓词表达式
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v)))) ; 遇到未约束的变量，就是错误 
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

;; 将谓词应用于实际参数
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

;;; always-true
(define (always-true ignore frame-stream)
  frame-stream)

(put 'always-true 'qeval always-true)


;;;;;;;;;;;;;;;;;;;
;; Pattern match ;;
;;;;;;;;;;;;;;;;;;;
(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame)) ; 丢掉不可能匹配的断言
                  (fetch-assertions pattern frame))) ; 从数据库获取断言的流
;; (add-rule-or-assertion! '(append-to-form (a b) (c d) (a b c d)))
;; (define test-frame (singleton-stream '()))
;; (find-assertions '(append-to-form (a b) (c d) (? z)) test-frame)
;; => ((((? z) a b c d)) . #[promise 129])

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame))) ; 调用匹配器
    (if (eq? match-result 'failed) 
        the-empty-stream ; 失败返回空流
        (singleton-stream match-result)))) ; 成功包含 一个扩充框架的流

;; (check-an-assertion '(append-to-form (a b) (c d) (a b c d)) '(append-to-form (a b) (c d) (? z)) '())
;; => ((((? z) a b c d)) . #[promise 134])

(define (pattern-match pat dat frame) 
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame) ; 相同时，匹配成功，直接返回原框架
        ((var? pat) (extend-if-consistent pat dat frame)) ; 模式是个变量，基于 frame 和新约束做扩充，检查是否协调
        ((and (pair? pat) (pair? dat)) 
         (pattern-match (cdr pat) ; 递归匹配模式和数据的cdr 部分
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame))) ; 以匹配 car 部分得到的可能，扩充的框架作为框架
        (else 'failed)))

;; (pattern-match '(append-to-form (a b) (c d) (? z)) '(append-to-form (a b) (c d) (a b c d)) '()) 
;; => (((? z) a b c d))

;; (pattern-match '(append-to-form (a b) (c d) (? z)) '(append-to-form (a b) (c d) (a b c d)) '(((? z) a b c))) ;; failed

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame))) ; 找出 var 在 frame 里的约束
    (if binding
        (pattern-match (binding-value binding) dat frame) ; 检查是否匹配 
        (extend var dat frame)))) ; var 无约束，把新约束加入frame

;; (extend-if-consistent '(? z) '(a b c d) '())
;; => (((? z) a b c d))
;; (binding-in-frame '(? z) '()) ; #f
;; (extend '(? z) '(a b c d) '()) ; (((? z) a b c d))

;; (extend-if-consistent '(? z) '(a b c d) '(((? z) a b c)))
;; failed
;; (binding-in-frame '(? z) '(((? z) a b c))) ; ((? z) a b c)
;; (binding-value '((? z) a b c)) ; (a b c)
;; (pattern-match '(a b c) '(a b c d) '(((? z) a b c))) ; failed

;;;;;;;;;;;
;; rules ;;
;;;;;;;;;;;
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

;; (add-rule-or-assertion! '(rule (append-to-form () (? y) (? y))))
;; (add-rule-or-assertion! '(rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z))))
;; (display-stream (apply-rules '(append-to-form (a b) (c d) (? z)) '()))
;; => 
;; (((? 71 z) c d) ((? 73 y) c d) ((? 70 z) (? 71 u) ? 71 z) ((? 71 y) c d) ((? 71 v)) ((? 71 u) . b) ((? z) (? 70 u) ? 70 z) ((? 70 y) c d) ((? 70 v) b) ((? 70 u) . a))

;; (define init-frame-stream (singleton-stream '())) 
;; (display-stream (fetch-rules '(append-to-form (a b) (c d) (? z)) init-frame-stream)) 
;; => (rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z)))
;;    (rule (append-to-form () (? y) (? y)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule))) ; 规则里的变量统一改名，使之不会与其他规则冲突
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame))) ; 做查询模式和规则结论做合一匹配
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result)))))) ; 基于得到的 新框架流 做 规则体 的匹配

;; (apply-a-rule '(rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z)))
;; 	      '(append-to-form (a b) (c d) (? z))
;; 	      '())
;; => ((((? 46 z) c d) ((? 48 y) c d) ((? 45 z) (? 46 u) ? 46 z) ((? 46 y) c d) ((? 46 v)) ((? 46 u) . b) ((? z) (? 45 u) ? 45 z) ((? 45 y) c d) ((? 45 v) b) ((? 45 u) . a)) . #[promise 81])

;; (rename-variables-in '(rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z))))
;; =>  (rule (append-to-form ((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z)) (append-to-form (? 38 v) (? 38 y) (? 38 z)))

;; (conclusion '(rule (append-to-form ((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z)) (append-to-form (? 38 v) (? 38 y) (? 38 z)))) ;; => (append-to-form ((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z))

;; (unify-match '(append-to-form (a b) (c d) (? z))
;;               '(append-to-form ((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z))        
;;              '())
;; => (((? z) (? 38 u) ? 38 z) ((? 38 y) c d) ((? 38 v) b) ((? 38 u) . a))

;; (rule-body '(rule (append-to-form ((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z)) (append-to-form (? 38 v) (? 38 y) (? 38 z)))) ;; (append-to-form (? 38 v) (? 38 y) (? 38 z))

;; (display-stream (qeval '(append-to-form (? 38 v) (? 38 y) (? 38 z))
;;        (singleton-stream '(((? z) (? 38 u) ? 38 z) ((? 38 y) c d) ((? 38 v) b) ((? 38 u) . a))))) 
;; => 
;; (((? 52 z) c d) ; ?-52z = (c d) 
;;  ((? 54 y) c d) ; ?-54y = (c d) 
;;  ((? 38 z) (? 52 u) ? 52 z) ; ?-38z = ?-52u . ?52-z
;;  ((? 52 y) c d) ; ?-52y = ( c d) 
;;  ((? 52 v))  ; ?-52v = () 
;;  ((? 52 u) . b) ; ?52-u = b 
;;  ((? z) (? 38 u) ? 38 z) ; ?z = (?-38u ?-38z) 
;;  ((? 38 y) c d) ; ?-38y = (c d) 
;;  ((? 38 v) b) ; ?-38v = (b) 
;;  ((? 38 u) . a)) ; ?-38u = a 

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

;; (rename-variables-in '(rule (append-to-form ((? u) ? v) (? y) ((? u) ? z)) (append-to-form (? v) (? y) (? z))))
;; =>  (rule (append-to-form ((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z)) (append-to-form (? 38 v) (? 38 y) (? 38 z)))

;;;;;;;;;;;;;;;;;;
;; Unifications ;;
;;;;;;;;;;;;;;;;;;
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame)) ; 两边都可能是变量
        ((var? p2) (extend-if-possible p2 p1 frame))  
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

;; (unify-match '(append-to-form (a b) (c d) (? z)) 
;;                         '(append-to-form ((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z)) 
;; 			'())
;; => (((? z) (? 38 u) ? 38 z) ((? 38 y) c d) ((? 38 v) b) ((? 38 u) . a))

;; (cdr '(append-to-form (a b) (c d) (? z))) ; ((a b) (c d) (? z))
;; (cdr '(append-to-form ((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z))) ;; (((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z))
;; (unify-match '((a b) (c d) (? z))
;; 	     '(((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z))
;; 	     (unify-match 'append-to-form 'append-to-form '()))

;; (unify-match 'append-to-form 'append-to-form '()) ; () 

;; (unify-match '((a b) (c d) (? z))
;; 	     '(((? 38 u) ? 38 v) (? 38 y) ((? 38 u) ? 38 z))
;; 	     '())

;; (unify-match '((c d) (? z))
;; 	     '((? 38 y) ((? 38 u) ? 38 z))
;; 	     (unify-match '(a b) '((? 38 u) ? 38 v) '()))

;; (unify-match '(a b) '((? 38 u) ? 38 v) '())
;; (unify-match '(b)
;; 	     '(? 38 v)
;; 	     (unify-match 'a
;; 			  '(? 38 u)
;; 			  '()))  

;; (unify-match 'a
;; 	     '(? 38 u)
;; 	     '()) 
;; (var? 'a) ; #f
;; (var? '(? 38 u)) ;#t 
;; (extend-if-possible '(? 38 u) 'a '()) ; (((? 38 u) . a))

;; (unify-match '(b)
;; 	     '(? 38 v)
;; 	     '(((? 38 u) . a))) ; (((? 38 v) b) ((? 38 u) . a))

;; (extend-if-possible '(? 38 v) '(b) '(((? 38 u) . a))) ;; (((? 38 v) b) ((? 38 u) . a))  

;; (unify-match '((c d) (? z))
;; 	     '((? 38 y) ((? 38 u) ? 38 z))
;; 	     '(((? 38 v) b) ((? 38 u) . a)))

;; (unify-match '((? z))
;; 	     '(((? 38 u) ? 38 z))
;; 	     (unify-match '(c d)
;; 			  '(? 38 y)
;; 			  '(((? 38 v) b) ((? 38 u) . a))))
;; => (((? z) (? 38 u) ? 38 z) ((? 38 y) c d) ((? 38 v) b) ((? 38 u) . a)) 

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding 
           (unify-match
            (binding-value binding) val frame)) ; 若var 已有约束，要求其约束值可与 val 合一    
          ((var? val)   ; 匹配的另一方也是变量。如果该变量有约束，则要求 var 可与该变量的约束值合一
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)     ; val 依赖于 var 时匹配失败
           'failed)
          (else (extend var val frame)))))

;; (extend-if-possible '(? 38 v) '(b) '(((? 38 u) . a))) ;; (((? 38 v) b) ((? 38 u) . a))  
;; (binding-in-frame '(? 38 v) '(((? 38 u) . a))) ; #f
;; (var? '(b)) ; #f 
;; (depends-on? '(? 38 v) '(b) '(((? 38 u) . a))) ; #f
;; (extend '(? 38 v) '(b) '(((? 38 u) . a))) ;; (((? 38 v) b) ((? 38 u) . a))

;; (extend-if-possible '(? 38 u) '(? 38 v) '(((? 38 u) . b)))) ;; (((? 38 v) . b) ((? 38 u) . b))
;; (extend-if-possible 'b '(? 38 v) '(((? 38 u) . a))) ;;  ((b ? 38 v) ((? 38 u) . a))

;; (extend-if-possible '(? 38 u) 'a '(((? 38 u) . b)))) ;; failed


;;; exp : 模式
;;; var: 变量
;;; frame: 框架
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e) ; 是变量
           (if (equal? var e)  ; 和 var 相同
               true
               (let ((b (binding-in-frame e frame))) ; b 是 e 在 frame 里的约束 
                 (if b
                     (tree-walk (binding-value b)) ; 检查约束值 是否有依赖
                     false))))
          ((pair? e) ; 递归检查每个序对的 car 和 cdr 
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;; (depends-on? 'a 'b '(((? 38 u) . a))) ; #f 
;; (depends-on? '(? 38 v) '(b) '(((? 38 u) . a))) ; #f

;; (depends-on? '(? 38 v) '(? 38 v) '(((? 38 u) . a))) ; #t
;; (depends-on? '(? 38 u) '(? 38 v) '(((? 38 u) . (? 38 v)))) ; #t 
;; (binding-in-frame '(? 38 u) '(((? 38 u) . (? 38 v)))) ; ((? 38 u) ? 38 v)  
;; (binding-value '((? 38 u) ? 38 v)) ; (? 38 v) 
;; (equal? '(? 38 v) (binding-value '((? 38 u) ? 38 v))) ; #t


;;;;;;;;;;;;;;;;;;;;;
;; query evalutor  ;;
;;;;;;;;;;;;;;;;;;;;;
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc 
        (qproc (contents query) frame-stream) ; 如果是特殊处理过程，就用该过程处理
        (simple-query query frame-stream)))) ; 非特殊形式的表达式都当作简单查询

;; (qeval '(append-to-form (a b) (c d) (? z)) (singleton-stream '()))
;; (type '(append-to-form (a b) (c d) (? z))) ; => append-to-form 
;; (get 'append-to-form 'qeval) ;#f 
;; (simple-query '(append-to-form (a b) (c d) (? z)) (singleton-stream '()))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp) ; 使用 frame 里的约束构造 exp 的实例化副本
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame)))) ; 用传入的 过程参数 来 处理未约束变量
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;; (instantiate '(append-to-form (a b) (c d) (? z)) 
;;                  '(((? 21 z) c d) ((? 23 y) c d) ((? 20 z) (? 21 u) ? 21 z) ((? 21 y) c d) ((? 21 v)) ((? 21 u) . b) ((? z) (? 20 u) ? 20 z) ((? 20 y) c d) ((? 20 v) b) ((? 20 u) . a))
;;                  (lambda (v f) 
;;                    (contract-question-mark v)))
;; ;; => (append-to-form (a b) (c d) (a b c d))

;; (instantiate '(? z)
;; 	      '(((? 21 z) c d) ((? 23 y) c d) ((? 20 z) (? 21 u) ? 21 z) ((? 21 y) c d) ((? 21 v)) ((? 21 u) . b) ((? z) (? 20 u) ? 20 z) ((? 20 y) c d) ((? 20 v) b) ((? 20 u) . a))
;;                  (lambda (v f) 
;;                    (contract-question-mark v))) 
;; ;; => (a b c d) 
;; (binding-in-frame '(? z)
;; 		  '(((? 21 z) c d) ((? 23 y) c d) ((? 20 z) (? 21 u) ? 21 z) ((? 21 y) c d) ((? 21 v)) ((? 21 u) . b) ((? z) (? 20 u) ? 20 z) ((? 20 y) c d) ((? 20 v) b) ((? 20 u) . a)))
;; ;; => ((? z) (? 20 u) ? 20 z)
;; (binding-value '((? z) (? 20 u) ? 20 z)) ;; ((? 20 u) ? 20 z)

;; (binding-in-frame '(? 20 u)
;; 		  '(((? 21 z) c d) ((? 23 y) c d) ((? 20 z) (? 21 u) ? 21 z) ((? 21 y) c d) ((? 21 v)) ((? 21 u) . b) ((? z) (? 20 u) ? 20 z) ((? 20 y) c d) ((? 20 v) b) ((? 20 u) . a))) ;; ((? 20 u) . a)
;; (binding-value '((? 20 u) . a)) ;; => a 

;; (binding-in-frame '(? 20 z)
;; 		  '(((? 21 z) c d) ((? 23 y) c d) ((? 20 z) (? 21 u) ? 21 z) ((? 21 y) c d) ((? 21 v)) ((? 21 u) . b) ((? z) (? 20 u) ? 20 z) ((? 20 y) c d) ((? 20 v) b) ((? 20 u) . a))) ;; => ((? 20 z) (? 21 u) ? 21 z)
;; (binding-value '((? 20 z) (? 21 u) ? 21 z)) ;; => ((? 21 u) ? 21 z)

;; (binding-in-frame '(? 21 u)
;; 		  '(((? 21 z) c d) ((? 23 y) c d) ((? 20 z) (? 21 u) ? 21 z) ((? 21 y) c d) ((? 21 v)) ((? 21 u) . b) ((? z) (? 20 u) ? 20 z) ((? 20 y) c d) ((? 20 v) b) ((? 20 u) . a))) ;; => ((? 21 u) . b)
;; (binding-value '((? 21 u) . b)) ;; => b

;; (binding-in-frame '(? 21 z)
;; 		  '(((? 21 z) c d) ((? 23 y) c d) ((? 20 z) (? 21 u) ? 21 z) ((? 21 y) c d) ((? 21 v)) ((? 21 u) . b) ((? z) (? 20 u) ? 20 z) ((? 20 y) c d) ((? 20 v) b) ((? 20 u) . a))) ;; => ((? 21 z) c d)
;; (binding-value '((? 21 z) c d)) ;; => (c d)
;; (cons 'a (cons 'b '(c d))) ;; => (a b c d) 


;; (instantiate '(append-to-form (a b) (c d) (? z)) 
;;              '(((? z) (? 20 u) ? 20 z) ((? 20 u) . a))
;;              (lambda (v f) 
;;                (contract-question-mark v)))
;; => (append-to-form (a b) (c d) (a . ?z-20))
;; (binding-in-frame '(? 20 z)
;; 		  '(((? z) (? 20 u) ? 20 z) ((? 20 u) . a))) ; #f
;; ((lambda (v f) ; 
;;    (contract-question-mark v))
;;  '(? 20 z) '(((? z) (? 20 u) ? 20 z) ((? 20 u) . a))) ;; => ?z-20

;;;;;;;;;;;;;;;;;;
;; driver loop  ;;
;;;;;;;;;;;;;;;;;;
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q)) ; 加入断言或规则
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop)) ; 重启主循环
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map 
             (lambda (frame)
               (instantiate q ; 用结果流中的框架 frame 做查询模式q 的实例化
                            frame
                            (lambda (v f) ; f 的实参是 frame 
                              (contract-question-mark v)))) ;处理未约束变量，产生适当的输出形式
             (qeval q (singleton-stream '())))) ; 从包含一个空框架的流查询出匹配的框架流
           (query-driver-loop))))) ; 重启主循环

;; (query-syntax-process '(append-to-form (a b) (c d) ?z)) 
;; => (append-to-form (a b) (c d) (? z))
;; (qeval '(append-to-form (a b) (c d) (? z)) (singleton-stream '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following are commented out so as not to be evaluated when ;;
;; the file is loaded.					      ;;
;; (query-driver-loop)					      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'LOGIC-METACIRCULAR-EVALUATOR-LOADED
