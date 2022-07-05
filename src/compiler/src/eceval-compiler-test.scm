(load "assembler") 
(load "eceval-support")
(load "compiler")
(load "eceval-compiler")

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
	1
	(* (factorial (- n 1)) n))))

;; (factorial 5)
;; 120
