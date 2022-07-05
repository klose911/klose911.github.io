;;;;;;;;;;;;;;;;;;;;
;; amb operations ;;
;;;;;;;;;;;;;;;;;;;;

(define (require p)									 
  (if (not p) (amb)))

(define (an-element-of items)							 
  (require (not (null? items))) ;; 表为空是计算失败					   
  (amb (car items) (an-element-of (cdr items)))) ;; 反之，返回表中任何一个元素		   

(define (prime-sum-pair list1 list2)							   
  (begin										   
    (define a (an-element-of list1))							   
    (define b (an-element-of list2))							   
    (require (prime? (+ a b)))								   
    (list a b)))									   

(prime-sum-pair '(1 3 5 8) '(20 35 110))						   
