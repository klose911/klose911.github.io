;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-traverse problem  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define call/cc call-with-current-continuation)

;;; native algorithm
(define flatten
  (lambda (tree)
    (cond ((null? tree) '())
          ((pair? (car tree))
           (append (flatten (car tree))
                   (flatten (cdr tree))))
          (else
           (cons (car tree)
                 (flatten (cdr tree)))))))

;; (flatten '(1 (2 3))) ; =>  (1 2 3)
;; (flatten '((1 2) 3)) ; =>  (1 2 3)

(define same-fringe?
  (lambda (tree1 tree2)
    (let loop ((ftree1 (flatten tree1))
               (ftree2 (flatten tree2)))
      (cond ((and (null? ftree1) (null? ftree2)) #t)
            ((or (null? ftree1) (null? ftree2)) #f)
            ((eqv? (car ftree1) (car ftree2))
             (loop (cdr ftree1) (cdr ftree2)))
            (else #f)))))

;; (same-fringe? '(1 (2 3)) '((1 2) 3)) ; => #t 
;; (same-fringe? '(a (b c) d) '(a b (d e))) ; => #f


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continuation solution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; named-let example

;; (define (fact-let n)
;;   (let loop((n1 n) (p n))           
;;     (if (= n1 1)                    
;; 	p
;; 	(let ((m (- n1 1)))
;; 	  (loop m (* p m))))))   
;; (fact-let 10) ; => 3628800

(define (tree->generator tree) ; 续延 0，返回给外层的比较函数
  (let ((caller '*)) ;; 初始化续延变量
    (letrec
        ((generate-leaves ; 内部定义的函数 generate-leaves // 续延 1，返回给 续延 0   
          (lambda ()
	    (let loop ((node tree)) ; named-let: 初始化 node 为 整课树 tree, 然后做遍历
              (cond ((null? node) 'skip) ; 到达底部，返回 'skip 
		    ((pair? node) ; 节点还是二叉树，顺序遍历 car, cdr 
		     (loop (car node))
		     (loop (cdr node)))
		    (else ; 节点是叶子
		     (call/cc ; 返回该叶节点作为生成器的结果，但是它会记住后续的循环并保存到 generate-leaves 变量
                      (lambda (rest-of-tree) ; 把续延 1 作为参数 rest-of-tree 传入
			(set! generate-leaves ; 重新设置 generate-leaves 过程
                              (lambda ()
				(rest-of-tree 'resume))) ; 记住续延1, ，下次调用 generate-leaves 时候，用来返回下一个访问到的节点
			(caller node)))))) ; 返回当前节点
	    (caller '())))) ; => 循环结束才会被调用，返回一个空列表给caller。由于空列表不是一个合法的叶节点，用它来告诉生成器没有叶节点需要生成了
      (lambda ()
	(call/cc
	 (lambda (k) ; 调用 tree->generator 的 续延0 被作为参数 k 传入
           (set! caller k) ; 局部变量 caller 被初始化为 续延0 
           (generate-leaves))))))) ; 初次调用 generate-leaves 过程 

;; (define generator (tree-generator '(a (b c) d)))  ;
;; (generator) ; => a 
;; (generator) ; => b
;; (generator) ; => c 
;; (generator) ; => d 
;; (generator) ; => () 
;; (generator) ; => () 

(define same-fringe?
  (lambda (tree1 tree2)
    (let ((gen1 (tree->generator tree1))
          (gen2 (tree->generator tree2)))
      (let loop ()
        (let ((leaf1 (gen1))
              (leaf2 (gen2)))
          (if (eqv? leaf1 leaf2)
              (if (null? leaf1) #t (loop))
              #f))))))

;; (define tree1 '(((a b) (y z)) (3 4)))
;; (define tree2 '(((a b) (t z)) (3 4)))
;; (define tree3 '(((a (b y) z)) (3 4)))
;; (same-fringe? tree1 tree2) ; => #f
;; (same-fringe? tree1 tree3) ; => #t

;;;;;;;;;;;;;;;;;;;;;;
;; 用函数实现协程序 ;;
;;;;;;;;;;;;;;;;;;;;;;

;;; queue(FIFO) 
(define (make-queue)
  (cons '() '()))

;;; 把一个元素加入到队列最后
(define (enqueue! queue obj)
  (let ((lobj (list obj)))
    (if (null? (car queue))
	(begin
	  (set-car! queue lobj)
	  (set-cdr! queue lobj))
	(begin
	  (set-cdr! (cdr queue) lobj)
	  (set-cdr! queue lobj)))
    (car queue)))

;;; 从队列最前面取出一个元素
(define (dequeue! queue)
  (let ((obj (car (car queue))))
    (set-car! queue (cdr (car queue)))
    obj))

;; (define test-queue (make-queue))
;; test-queue ;=>  (())
;; (enqueue! test-queue 'a)
;; test-queue ;=>   ((a) a)
;; enqueue! test-queue 'b)
;; test-queue ;=>   ((a b) b) 
;; (enqueue! test-queue 'c)
;; test-queue ;=>   ((a b c) c)
;; (dequeue! test-queue) ; => a 
;; test-queue ; => ((b c) c)
;; (dequeue! test-queue) ; => b 
;; test-queue ; => ((c) c)
;; (dequeue! test-queue) ; => a
;; test-queue ; => (() c) 


;;; coroutine
(define process-queue (make-queue)) ; 协程队列

;;; 把一个协程添加到队列最后
(define (coroutine thunk) 
  (enqueue! process-queue thunk))

;;; 获得队列中的第一个元素，并执行它 
(define (start)
  ((dequeue! process-queue)))


(define (pause)
  (call/cc
   (lambda (k) ; 当前续延作为参数 k 传入
     (coroutine (lambda () (k #f))) ; 添加当前续延到最后
     (start)))) ; 执行当前队列第一个元素

  ;;; example
;; (coroutine (lambda ()
;;              (let loop ((i 0)) 
;;                (if (< i 10)
;; 		   (begin
;; 		     (display (1+ i)) 
;; 		     (display " ")
;; 		     (pause) 
;; 		     (loop (1+ i)))))))

;; (coroutine (lambda ()
;;              (let loop ((i 0)) 
;;                (if (< i 10)
;; 		   (begin
;; 		     (display (integer->char (+ i 97)))
;; 		     (display " ")
;; 		     (pause) 
;; 		     (loop (1+ i)))))))

;; (newline)
;; (start)
;; 1 a 2 b 3 c 4 d 5 e 6 f 7 g 8 h 9 i 10 j

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 创建协程宏：返回一个函数，一个变量名作为协程函数的初始参数，内容作为协程函数的内容 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require compatibility/defmacro)

;; (define-macro coroutine
;;   (lambda (x . body) ; x ：协程A的初始参数，body：协程A的内容
;;     `(letrec ((+local-control-state (lambda (,x) ,@body)) ; 运行协程
;;               (resume ; 两个参数的函数，保存协程A的续延，转而执行协程B 
;;                (lambda (c v) ; c: 另外一个协程B，v：执行用的参数
;;                  (call/cc
;;                   (lambda (k) ; 当前续延作为参数传入
;;                     (set! +local-control-state k) ; 把当前续延保存到 +local-control-state 
;;                     (c v)))))) ; 执行协程 B 
;;        (lambda (v)
;;          (+local-control-state v))))) ; 协程A恢复后，会从local-control-state变量里存放的续延开始

;; ;;; tree->generator 协程: tree遍历的树，matcher-cor 比较叶子节点的协程 
;; (define (make-leaf-gen-coroutine tree matcher-cor) 
;;   (coroutine dont-need-an-init-arg ; 任意参数
;; 	     (let loop ((node tree))
;; 	       (cond ((null? node) 'skip)
;; 		     ((pair? node)
;; 		      (loop (car node))
;; 		      (loop (cdr node)))
;; 		     (else
;; 		      (resume matcher-cor node)))) ; 转而执行 matcher-cor 协程做比较，传递给 matcher-cor 的参数是当前的叶子节点 node   
;; 	     (resume matcher-cor '()))) ; 转而执行 matcher-cor 协程，传递给 matcher-cor 的参数是空列表，通知 macher-cor 协程遍历完毕

;; ;;; 比较叶子节点的协程：tree-cor-1 遍历树1的协程， tree-cor-2 遍历树2的协程
;; (define (make-matcher-coroutine tree-cor-1 tree-cor-2) 
;;   (coroutine dont-need-an-init-arg ; 任意参数
;; 	     (let loop ()
;; 	       (let ((leaf1 (resume tree-cor-1 'get-1-a-leaf)) ; 转而执行 tree-cor-1 协程， 获取第一颗树的当前叶子节点，传递的参数可以任意
;; 		     (leaf2 (resume tree-cor-2 'get-2-a-leaf))) ; 转而执行 tree-cor-2 协程， 获取第二颗树的当前叶子节点，传递的参数可以任意
;; 		 (if (eqv? leaf1 leaf2)
;; 		     (if (null? leaf1) #t (loop))
;; 		     #f)))))

;; (define (same-fringe? tree1 tree2) 
;;   (letrec ((tree-cor-1 ; 创建 遍历第一颗树 协程
;;             (make-leaf-gen-coroutine
;;              tree1
;;              (lambda (v) (matcher-cor v))))
;;            (tree-cor-2 ; 创建 遍历第二颗树 协程
;;             (make-leaf-gen-coroutine
;;              tree2
;;              (lambda (v) (matcher-cor v))))
;;            (matcher-cor ; 创建 比较叶子节点 协程
;;             (make-matcher-coroutine
;;              (lambda (v) (tree-cor-1 v))
;;              (lambda (v) (tree-cor-2 v)))))
;;     (matcher-cor 'start-ball-rolling)))

;; (define tree1 '(((a b) (y z)) (3 4)))
;; (define tree2 '(((a b) (t z)) (3 4)))
;; (define tree3 '(((a (b y) z)) (3 4)))
;; (same-fringe? tree1 tree2) ; => #f
;; (same-fringe? tree1 tree3) ; => #t

;;;;;;;;;;;;
;; 卫生宏 ;;
;;;;;;;;;;;;
(define-syntax coroutine
  (syntax-rules ()
    ((coroutine arg resume body ...) ; 故意把 resume 放到了 coroutine 宏的参数列表中。创建协程时候，必须使用一个符号作为局部函数resume的名字
     (letrec ((local-control-state
               (lambda (arg) body ...))
              (resume
               (lambda (c v)
                 (call/cc
                  (lambda (k)
                    (set! local-control-state k)
                    (c v))))))
       (lambda (v)
         (local-control-state v))))))

(define (make-leaf-gen-coroutine tree matcher-cor) 
  (coroutine dont-need-an-init-arg
	     go-on ; 任意给定一个名字， 但下面必须使用这个名字来执行宏中resume的
	     (let loop ((node tree))
	       (cond ((null? node) 'skip)
		     ((pair? node)
		      (loop (car node))
		      (loop (cdr node)))
		     (else
		      (go-on matcher-cor node))))
	     (go-on matcher-cor '()))) 

(define (make-matcher-coroutine tree-cor-1 tree-cor-2) 
  (coroutine dont-need-an-init-arg 
	     go-on ; 任意给定一个名字, 但下面必须使用这个名字来执行宏中resume的
	     (let loop ()
	       (let ((leaf1 (go-on tree-cor-1 'get-1-a-leaf)) 
		     (leaf2 (go-on tree-cor-2 'get-2-a-leaf))) 
		 (if (eqv? leaf1 leaf2)
		     (if (null? leaf1) #t (loop))
		     #f)))))

(define (same-fringe? tree1 tree2) 
  (letrec ((tree-cor-1 
            (make-leaf-gen-coroutine
             tree1
             (lambda (v) (matcher-cor v))))
           (tree-cor-2 
            (make-leaf-gen-coroutine
             tree2
             (lambda (v) (matcher-cor v))))
           (matcher-cor 
            (make-matcher-coroutine
             (lambda (v) (tree-cor-1 v))
             (lambda (v) (tree-cor-2 v)))))
    (matcher-cor 'start-ball-rolling)))

;; (define tree1 '(((a b) (y z)) (3 4)))
;; (define tree2 '(((a b) (t z)) (3 4)))
;; (define tree3 '(((a (b y) z)) (3 4)))
;; (same-fringe? tree1 tree2) ; => #f
;; (same-fringe? tree1 tree3) ; => #t
