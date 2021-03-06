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

(define (conclusion rule) (cadr rule)) ; ??????

;; (conclusion  '(rule (wheel ?person)
;;       (and (supervisor ?middle-manager ?person)
;; 	   (supervisor ?x ?middle-manager)))) ; (wheel ?person)

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule))) ; ????????????

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
  (let ((chars (symbol->string symbol))) ; ?????? symbol ??????????????????
    (if (string=? (substring chars 0 1) "?") ; ?????????????????????????????? '?
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars)))) ; ?????? symbol ??????????????? '? ???????????????
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
		  (if (number? (cadr variable)) ; ??????????????????????????????????????????????????????
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
  (or (constant-symbol? (car pat)) ; ????????? car ???????????????
      (var? (car pat)))) ; ????????????????????? car ???????????????

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
  (if (use-index? pattern) ; pattern ??? car ?????????
      (get-indexed-assertions pattern) ; ???????????????????????????
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
   (get-stream '? 'rule-stream))) ; ??????????????? car ???????????????????????? ??????? ??????

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
  (stream-flatmap ; ???????????????????????????????????????????????? 
   (lambda (frame) 
     (stream-append-delayed ; ???????????????
      (find-assertions query-pattern frame) ; ????????????????????????????????????????????????????????? 
      (delay (apply-rules query-pattern frame)))) ; ??????????????????????????????????????????????????? 
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
     (if (stream-null? (qeval (negated-query operands) ; ???????????????
			      (singleton-stream frame))) ; ??????????????????????????????????????? ?????????
         (singleton-stream frame) ; ??????????????????????????????
         the-empty-stream)) ; ???????????????????????????
   frame-stream))

(put 'not 'qeval negate) 

;;; lisp-value filters
(define (lisp-value call frame-stream) ; call ??????????????????
  (stream-flatmap
   (lambda (frame)
     (if (execute ; ??????????????????????????????????????? eval?????????????????????????????????????????????????????????
          (instantiate ; instantiate ??? frame ????????? call ?????????????????????????????????????????????
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v)))) ; ??????????????????????????????????????? 
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

;; ??????????????????????????????
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
                    (check-an-assertion datum pattern frame)) ; ??????????????????????????????
                  (fetch-assertions pattern frame))) ; ??????????????????????????????
;; (add-rule-or-assertion! '(append-to-form (a b) (c d) (a b c d)))
;; (define test-frame (singleton-stream '()))
;; (find-assertions '(append-to-form (a b) (c d) (? z)) test-frame)
;; => ((((? z) a b c d)) . #[promise 129])

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame))) ; ???????????????
    (if (eq? match-result 'failed) 
        the-empty-stream ; ??????????????????
        (singleton-stream match-result)))) ; ???????????? ????????????????????????

;; (check-an-assertion '(append-to-form (a b) (c d) (a b c d)) '(append-to-form (a b) (c d) (? z)) '())
;; => ((((? z) a b c d)) . #[promise 134])

(define (pattern-match pat dat frame) 
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame) ; ????????????????????????????????????????????????
        ((var? pat) (extend-if-consistent pat dat frame)) ; ??????????????????????????? frame ??????????????????????????????????????????
        ((and (pair? pat) (pair? dat)) 
         (pattern-match (cdr pat) ; ??????????????????????????????cdr ??????
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame))) ; ????????? car ???????????????????????????????????????????????????
        (else 'failed)))

;; (pattern-match '(append-to-form (a b) (c d) (? z)) '(append-to-form (a b) (c d) (a b c d)) '()) 
;; => (((? z) a b c d))

;; (pattern-match '(append-to-form (a b) (c d) (? z)) '(append-to-form (a b) (c d) (a b c d)) '(((? z) a b c))) ;; failed

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame))) ; ?????? var ??? frame ????????????
    (if binding
        (pattern-match (binding-value binding) dat frame) ; ?????????????????? 
        (extend var dat frame)))) ; var ??????????????????????????????frame

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
  (let ((clean-rule (rename-variables-in rule))) ; ??????????????????????????????????????????????????????????????????
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame))) ; ?????????????????????????????????????????????
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result)))))) ; ??????????????? ???????????? ??? ????????? ?????????

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
        ((var? p1) (extend-if-possible p1 p2 frame)) ; ????????????????????????
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
            (binding-value binding) val frame)) ; ???var ??????????????????????????????????????? val ??????    
          ((var? val)   ; ????????????????????????????????????????????????????????????????????? var ?????????????????????????????????
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)     ; val ????????? var ???????????????
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


;;; exp : ??????
;;; var: ??????
;;; frame: ??????
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e) ; ?????????
           (if (equal? var e)  ; ??? var ??????
               true
               (let ((b (binding-in-frame e frame))) ; b ??? e ??? frame ???????????? 
                 (if b
                     (tree-walk (binding-value b)) ; ??????????????? ???????????????
                     false))))
          ((pair? e) ; ??????????????????????????? car ??? cdr 
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
        (qproc (contents query) frame-stream) ; ???????????????????????????????????????????????????
        (simple-query query frame-stream)))) ; ????????????????????????????????????????????????

;; (qeval '(append-to-form (a b) (c d) (? z)) (singleton-stream '()))
;; (type '(append-to-form (a b) (c d) (? z))) ; => append-to-form 
;; (get 'append-to-form 'qeval) ;#f 
;; (simple-query '(append-to-form (a b) (c d) (? z)) (singleton-stream '()))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp) ; ?????? frame ?????????????????? exp ??????????????????
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame)))) ; ???????????? ???????????? ??? ?????????????????????
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
           (add-rule-or-assertion! (add-assertion-body q)) ; ?????????????????????
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop)) ; ???????????????
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map 
             (lambda (frame)
               (instantiate q ; ???????????????????????? frame ???????????????q ????????????
                            frame
                            (lambda (v f) ; f ???????????? frame 
                              (contract-question-mark v)))) ;???????????????????????????????????????????????????
             (qeval q (singleton-stream '())))) ; ?????????????????????????????????????????????????????????
           (query-driver-loop))))) ; ???????????????

;; (query-syntax-process '(append-to-form (a b) (c d) ?z)) 
;; => (append-to-form (a b) (c d) (? z))
;; (qeval '(append-to-form (a b) (c d) (? z)) (singleton-stream '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following are commented out so as not to be evaluated when ;;
;; the file is loaded.					      ;;
;; (query-driver-loop)					      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'LOGIC-METACIRCULAR-EVALUATOR-LOADED
