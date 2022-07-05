   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;		      frac.scm
   ;;;
   ;;;	      draw fractal curves
   ;;;	       by T.Shido
   ;;;	     on August 20, 2005
   ;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 平面直角坐标系上的点通过序对来表示，其中car部分和cdr部分分别代表
;;; x坐标和y坐标。
;;; 函数_x和_y用来取得坐标，point用来建立一个点
(define _x car)
(define _y cdr)
(define point cons)

;;; 接受两个表作为参数，将第一个表反转后与第二个表连接起来。
(define (rappend ls0 ls1)
  (let loop((ls0 ls0) (ls1 ls1))
    (if (null? ls0)
	ls1
	(loop (cdr ls0) (cons (car ls0) ls1)))))

;; (rappend '(1 2 3) '(4 5 6)) -> (3 2 1 4 5 6)

;;; (devide p1 p2 r)
;;; dividing p1 and p2 internally by the ratio r
(define (divide p1 p2 r)
  (point (+ (* r (_x p1)) (* (- 1.0 r) (_x p2)))
	 (+ (* r (_y p1)) (* (- 1.0 r) (_y p2)))))

;;; 将点输出至文件。将一系列点points按一行一个点得格式输出至fout代表的文件
(define (print-curve points fout)
  (with-output-to-file fout
    (lambda ()
      (for-each
       (lambda (p)
	 (display (_x p))
	 (display " ")
	 (display (_y p))
	 (newline))
       points))))

;;; (fractal proc n points fout)
;;; 创建分型图形的高阶函数。其中，proc是定位函数，n是重复次数
;;; points是初始点构成的表，fout是输出文件的文件名
(define (fractal proc n points fout)
  (let loop((i 0) (points points)) ;; loop对数据表做n次插入
    (if (= n i)
	(print-curve points fout)
	(loop
	 (1+ i)
	 (let iter ((points points) (acc '())) ;;  iter 调用 proc 函数把点加入一个列表
	   (if (null? (cdr points)) 
	       (reverse! (cons (car points) acc))
	       (iter
		(cdr points)
		(rappend (proc (first points) (second points)) acc))))))) ;; proc 函数有2个参数，两个点，返回第一个点和中间点的列表
  'done)

;;; c curve
(define (c-curve p1 p2) 
  (let ((p3 (divide p1 p2 0.5)))
    (list
     p1
     (point (+ (_x p3) (- (_y p3) (_y p2)))
	    (+ (_y p3) (- (_x p2) (_x p3)))))))

;;; dragon curve
(define dragon-curve 
  (let ((n 0))
    (lambda (p1 p2)
      (let ((op (if (even? n) + -))
	    (p3 (divide	 p1 p2 0.5)))
	(set! n (1+ n))
	(list
	 p1
	 (point (op (_x p3) (- (_y p3) (_y p2)))
		(op (_y p3) (- (_x p2) (_x p3)))))))))

;;; koch curve
(define (koch p1 p2)
  (let ((p3 (divide p1 p2 2/3))
	(p4 (divide p1 p2 1/3))
	(p5 (divide p1 p2 0.5))
	(c  (/ (sqrt 3) 2)))
    (list
     p1
     p3
     (point (- (_x p5) (* c (- (_y p4) (_y p3))))
	    (+ (_y p5) (* c (- (_x p4) (_x p3)))))
     p4)))

;; (compile-file "frac.scm")
;; (load "frac")

;; C-Curve
(fractal c-curve 14 '((0 . 0) (2 . 3)) "c14.dat")

;; Dragon-Curve
(fractal dragon-curve 14 '((0 . 0) (1 . 0)) "d14.dat")

;; Koch-Curve
(fractal koch 5 '((0 . 0) (1 . 0)) "k5.dat")
