;;; function 
(defun hello-world (name)
  "Say hello to user whose name is NAME."
  (message "Hello, %s" name))

(hello-world "Emacser")                 ; => "Hello, Emacser"

;;; global variables 
(setq foo "I'm foo")                    ; => "I'm foo"
(message foo)                           ; => "I'm foo"

(defvar foo "Did I have a value?"
  "A demo variable")                    ; => foo

foo                                     ; => "I'm foo"

(defvar bar "I'm bar"
  "A demo variable named \"bar\"")      ; => bar

bar

;;; local varibales 
(defun circle-area (radix)
  (let ((local-pi 3.1415926)
        area)
    (setq area (* local-pi radix radix))
    (message "直径为 %.2f 的圆面积是 %.2f" radix area)))

(circle-area 3) ;; => 直径为 3.00 的圆面积是 28.27

;; (defun circle-area (radix)
;;     (let* ((local-pi 3.1415926)
;; 	   (area (* local-pi radix radix)))
;;       (message "直径为 %.2f 的圆面积是 %.2f" radix area))


;;; lambda expression 
(funcall (lambda (name)
	   (message "Hello, %s!" name)) "Emacser")

(setq foo (lambda (name)
            (message "Hello, %s!" name)))

(funcall foo "Emacser")                   ; => "Hello, Emacser!"

;;;;;;;;;;;;;;;;;;
;; control flow ;;
;;;;;;;;;;;;;;;;;;
;;; sequential
(progn
  (setq foo 3)
  (message "Square of %d is %d" foo (* foo foo)))

;;; condition
(defun my-max (a b)
  (if (> a b)
      a b))
(my-max 3 4)                            ; => 4

(defun fib (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (fib (- n 1))
	      (fib (- n 2))))))
(fib 10)                                ; => 55

;;; loop
(defun factorial (n)
  (let ((res 1))
    (while (> n 1)
      (setq res (* res n)
            n (- n 1)))
    res))
(factorial 10)      ;; => 55 

;;;;;;;;;;;;;;;;;;;;;
;; logic operator  ;;
;;;;;;;;;;;;;;;;;;;;;

(defun hello-world (&optional name)
  (or name (setq name "Emacser"))
  (message "Hello, %s" name))           ; => hello-world
(hello-world)                           ; => "Hello, Emacser"
(hello-world "Ye")                      ; => "Hello, Ye"

(defun square-number-p (n)
  (and (>= n 0)
       (= (/ n (sqrt n)) (sqrt n))))
(square-number-p -1)                    ; => nil
(square-number-p 25)                    ; => t
