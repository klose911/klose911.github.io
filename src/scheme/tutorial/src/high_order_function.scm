;;;;;;;;;;;;;;;;;;;;;;;;;
;; high order function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(sort '(7883 9099 6729 2828 7754 4179 5340 2644 2958 2239) <)
;; (2239 2644 2828 2958 4179 5340 6729 7754 7883 9099)

(sort '(7883 9099 6729 2828 7754 4179 5340 2644 2958 2239) 
      (lambda (x y) (< (modulo x 100) (modulo y 100))))
;; (2828 6729 2239 5340 2644 7754 2958 4179 7883 9099)

;;;;;;;;;;;;;
;; Mapping ;;
;;;;;;;;;;;;;

;;; map
;; Adding each item of '(1 2 3) and '(4 5 6).
(map + '(1 2 3) '(4 5 6)) ; (5 7 9) 

;; Squaring each item of '(1 2 3)
(map (lambda (x) (* x x)) '(1 2 3)) ; (1 4 9)

;;; for-each
(define sum 0)
(for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3 4))
sum ;  10

;;;;;;;;;;;;
;; Filter ;;
;;;;;;;;;;;;
(keep-matching-items '(1 2 -3 -4 5) positive?) ;  (1 2 5) 

;;;;;;;;;;;;
;; Reduce ;;
;;;;;;;;;;;;
(reduce + 0 '(1 2 3 4))                 ;  10
(reduce + 0 '(1 2))                     ;  3
(reduce + 0 '(1))                       ;  1
(reduce + 0 '())                        ;  0
(reduce + 0 '(foo))                     ;  foo
(reduce list '() '(1 2 3 4))            ;  (4 (3 (2 1))) 

;;;;;;;;;;
;; Sort ;;
;;;;;;;;;;
(sort '(3 5 1 4 -1) <) ; (-1 1 3 4 5)

;;;;;;;;;;;
;; Apply ;;
;;;;;;;;;;;
(apply max '(1 3 2))      ;  3
(apply + 1 2 '(3 4 5))    ;  15
(apply - 100 '(5 12 17))  ;  66

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define High Order Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; memeber-if
(define (member-if proc ls)
  (cond
   ((null? ls) #f)
   ((proc (car ls)) ls)
   (else (member-if proc (cdr ls)))))

(member-if positive? '(0 -1 -2 3 5 -7)) ; (3 5 -7)

;;; member
(define (member proc obj ls)
  (cond
   ((null? ls) #f)
   ((proc obj (car ls)) ls)
   (else (member proc obj (cdr ls)))))

(member string=? "hello" '("hi" "guys" "bye" "hello" "see you")) ; ("hello" "see you")
