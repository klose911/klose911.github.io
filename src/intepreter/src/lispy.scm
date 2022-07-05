(define NIL '()) 

(define (nil? x)
  (eq? x NIL))

(define (atom? x)
  (cond ((pair? x) #f)
	(else #t))) 

(define (equal*? x y)
  (cond ((atom? x) (cond ((atom? y) (eq? x y))
			 (else #f)))
	((equal*? (car x) (car y)) (equal*? (cdr x) (cdr y)))
	(else #f))) 

(equal*? '(A . B) '(A . B))

(define (subst x y z)
  (cond ((equal*? y z) x)
	((atom? z) z)
	(else (cons (subst x y (car z)) (subst x y (cdr z)))))) 

(subst '(X . A) 'B '((A . B) . C)) ;; '((A . (X . A)) . C) 

(define (my-append x y)
  (cond ((nil? x) y)
	(else (cons (car x) (my-append (cdr x) y)))))

(my-append '(A B) '(C D E)) ;; (A B C D E)

(define (my-member x y)
  (cond ((nil? y) #f)
	((equal*? x (car y)) #t)
	(else (my-member x (cdr y))))) 

(define (pairlis x y a)
  (cond ((nil? x) a)
	(else (cons (cons (car x) (car y))
		    (pairlis (cdr x) (cdr y) a)))))

(pairlis '(A B C) '(U V W) '((D . X) (E . Y))) ;; ((A . U) (B . V) (C . W) (D . X) (E . Y))

;; x is key
;; a is an asscocation list
;; return  pair result 
(define (my-assoc x a)
  (cond ((equal*? (caar a) x) (car a))
	(else (my-assoc x (cdr a)))))

;;(my-assoc 'B '((A. (M N)) (B . (CAR X)) (C . (QUOTE M)) (C . (CDR X)))) ;; (B . (CAR X) 

(define (sub2 a z)
  (cond ((nil? a) z)
	((eq? (caar a) z) (cdar a))
	(else (sub2 (cdr a) z))))

(sub2 '((A. (M N)) (B . (CAR X)) (C . (QUOTE M)) (C . (CDR X))) 'B) ;; (CAR X) 

;; a: an assocation list
;; y: S Expression
;; replace y with the key/value pair of a 
(define (sublis a y)
  (cond ((atom? y) (sub2 a y))
	(else (cons (sublis a (car y))
		    (sublis a (cdr y))))))

(sublis '((X . SHAKESPARE) (Y . (THE TEMPEST))) '(X WROTE Y)) ;; (SHAKESPARE WROTE (THE TEMPEST))

(define (my-eval e a)
  (cond ((atom? e) (my-assoc e a))
	((atom? (car e)) (cond ((eq? (car e) 'QUOTE) (cadr e))
			       ((eq? (car e) 'COND) (evcon (cdr e) a))
			       (else (my-apply (car e) (evlis (cdr e) a) a))))
	(else (my-apply (car e) (evlis (cdr e) a) a))))

(define (my-apply fn x a)
  (cond ((atom? fn) (cond ((eq? fn 'CAR) (caar x))
			  ((eq? fn 'CDR) (cdar x))
			  ((eq? fn 'CONS) (cons (car x) (cadr x)))
			  ((eq? fn 'ATOM?) (atom? (car x)))
			  ((eq? fn 'EQ?) (eq? (car x) (cadr x)))
			  (else (my-apply (my-eval fn a) x a))))
	((eq? (car fn) 'LAMBDA) (my-eval (caddr fn) (pairlis (cadr fn) x a)))
	((eq? (car fn) 'DEFINE) (my-apply (caddr fn) x (cons (cons (cadr fn) (caddr fn)) a)))))

(define (evcon c a)
  (cond ((my-eval (caar c) a) (my-eval (cadar c) a))
	(else (envcon (cdr c) a))))

(define (evlis m a)
  (cond ((null? m) NIL)
	(else (cons (my-eval (car m) a) (evlis (cdr m) a)))))

(my-apply '(LAMBDA (x y) (CONS (CAR x) y)) '((A B) (C D)) NIL)	
(car '(LAMBDA (x y) (CONS (CAR x) y))) ;; LAMBDA
(caddr '(LAMBDA (x y) (CONS (CAR x) y))) ;;  (CONS (CAR x) y)
(pairlis (cadr '(LAMBDA (x y) (CONS (CAR x) y))) '((A B) (C D)) NIL) ;; ((x A B) (y C D))

(my-eval '(CONS (CAR x) y) '((x A B) (y C D))) ;; (x y C D) 

