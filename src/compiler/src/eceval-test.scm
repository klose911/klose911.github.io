;;;; LOADS THE EXPLICIT-CONTROL EVALUATOR 
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS, WITH
;;;; ALL THE SUPPORTING CODE IT NEEDS IN ORDER TO RUN.

(load "assembler")		;reg machine simulator
(load "eceval-support")		;simulation of machine operations
(load "eceval")			;eceval itself

(define the-global-environment (setup-environment)) 
;; (start eceval) 

;;; EC-Eval input:
;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x)
;; 	    (append (cdr x) y)))) 

;;; EC-Eval value:
;; => ok

;;; EC-Eval input:
;; (append '(a b c) '(d e f)) 

;;; EC-Eval value:
;; => (a b c d e f)
