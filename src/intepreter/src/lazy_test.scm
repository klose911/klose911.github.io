(define (try a b)
  (if (> a 0) 1 b))

;; (try 0 (/ 1 0)) ;Division by zero signalled by /.

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

;; (define a 10)
;; (define b 2)
;; (unless (= b 0)
;;         (/ a b)
;;         (begin (display "exception: returning 0")
;;                0))
;; => exception: returning 0
;; Value: 5

;;; stream test 
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

;; (define ones (cons 1 ones))
;; (define integers (cons 1 (add-lists ones integers)))

;;; L-Eval input:
;; (list-ref integers 17) 
;;; L-Eval value:
;;  18

;;; integral test 
;; (define (integral integrand initial-value dt)
;;   (define int
;;     (cons initial-value
;;           (add-lists (scale-list integrand dt)
;;                      int)))
;;   int)

;; (define (solve f y0 dt)
;;   (define y (integral dy y0 dt))
;;   (define dy (map f y))
;;   y)

;;; L-Eval input:
;; (list-ref (solve (lambda (x) x) 1 0.001) 1000)
;;; L-Eval value:
;; 2.716923932235896
