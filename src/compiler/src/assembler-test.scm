(load "assembler")

(define gcd-machine
  (make-machine
   '(a b t) ;; 寄存器列表
   (list (list 'rem remainder) (list '= =)) ;; 操作列表
   '(test-b ;; 控制器代码
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 206)
;; done
(set-register-contents! gcd-machine 'b 40)
;; done
(start gcd-machine)
;; done
(get-register-contents gcd-machine 'a)
;; => 2


(gcd-machine 'operations)

;; (assemble '(assign b (reg a)) gcd-machine)

(car '(assign b (cons 0))) ;; assign 
(assign-reg-name '(assign b (cons 0)))  ;; b
(assign-value-exp '(assign b (cons 0)))
;; => ((cons 0))

(test-condition '(test (op =) (reg b) (const 0)))
;; => ((op =) (reg b) (const 0)) 

(branch-dest '(branch (label gcd-done)))
;; => (label gcd-done) 

(perform-action '(perform (op print) (reg a)))

(operation-exp? '((op =) (reg b) (const 0))) ;; #t 
(operation-exp-op '((op =) (reg b) (const 0))) ;; =
(operation-exp-operands '((op =) (reg b) (const 0)))
;; => ((reg b) (const 0))


