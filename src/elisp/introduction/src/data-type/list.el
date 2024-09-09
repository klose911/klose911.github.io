;;;;;;;;;;;;;;;;
;; list       ;;
;;;;;;;;;;;;;;;;

;;; read 
'(1 . 2)                                ; => (1 . 2)
'(?a . 1)                               ; => (97 . 1)
'(1 . "a")                              ; => (1 . "a")
'(1 . nil)                              ; => (1)
'(nil . nil)                            ; => (nil)

(read "(1 . 2)")                        ; => (1 . 2)

;;; empty list 
nil                                     ; => nil
'()                                     ; => nil

(car nil)                               ; => nil
(cdr nil)                               ; => nil

;;; type
'(1 2 3)                                  ; => (1 2 3)
'(1 2 . 3)                                ; => (1 2 . 3)
'(1 . #1=(2 3 . #1#))                     ; => (1 2 3 . #1)


'(1 . (2 . (3 . nil)))                  ; => (1 2 3)

;;; test function
(consp '(1 . 2))                        ; => t
(consp '(1 . (2 . nil)))                ; => t
(consp nil)                             ; => nil
(listp '(1 . 2))                        ; => t
(listp '(1 . (2 . nil)))                ; => t
(listp nil)                             ; => t

(null '()) ;; => t
(null nil) ;; => t
(null '(1 2) ;; => nil

      
;;; constructor

;; cons 
(cons 1 2)                              ; => (1 . 2)
(cons 1 '())                            ; => (1)

(setq foo '(a b))                       ; => (a b)
(cons 'x foo)                           ; => (x a b)
foo
					; => (a b)
(push 'x foo)                           ; => (x a b)
foo                                     ; => (x a b)

;; list 
(list 1 2 3)                            ; => (1 2 3)

;; quote
'((+ 1 2) 3)                            ; => ((+ 1 2) 3)
(list (+ 1 2) 3)                        ; => (3 3)

;; append
(append '(a b) '(c))                    ; => (a b c)
(append '(a b) '(c) '(d))               ; => (a b c d)
(append '(a b) 'c)                      ; => (a b . c)

(append [a b] "cd" nil)                 ; => (a b 99 100)

;;;;;;;;;;;;
;; array  ;;
;;;;;;;;;;;;
(nth 3 '(0 1 2 3 4 5))                  ; => 3
(nthcdr 2 '(0 1 2 3 4 5))               ; => (2 3 4 5)
(last '(0 1 2 3 4 5) 2)                 ; => (4 5)
(butlast '(0 1 2 3 4 5) 2)              ; => (0 1 2 3)

;;;;;;;;;;;;;
;; modify  ;;
;;;;;;;;;;;;;
(setq foo '(a b c))                     ; => (a b c)
(setcar foo 'x)                         ; => x
foo                                     ; => (x b c)
(setcdr foo '(y z))                     ; => (y z)
foo                                     ; => (x y z)

(setq foo '(a b c))                     ; => (a b c)
(setcdr foo foo)
foo ; => (a . #0) 这里的 #0 代表的其实是foo这个变量在内存中的地址

(setq foo '(1 2 3))                     ; => (1 2 3)
(setcar foo 'a)                         ; => a
(setcar (cdr foo) 'b)                   ; => b
(setcar (nthcdr 2 foo) 'c)              ; => c
foo                                     ; => (a b c)

;;;;;;;;;;;;
;; stack  ;;
;;;;;;;;;;;;
(setq foo nil)                          ; => nil
(push 'a foo)                           ; => (a)
(push 'b foo)                           ; => (b a)
(pop foo)                               ; => b
foo                                     ; => (a)

;;;;;;;;;;;
;; sort  ;;
;;;;;;;;;;;
(setq foo '(a b c))                     ; => (a b c)
(reverse foo)                           ; => (c b a)
foo                                     ; => (a b c)

(nreverse foo)                          ; => (c b a)
foo                                     ; => (a) 原列表已经被破坏了!!!

(setq foo '(3 2 4 1 5))                 ; => (3 2 4 1 5)
(sort foo '<)                           ; => (1 2 3 4 5)
foo                                     ; => (3 4 5)

;;;;;;;;;;;;;;;;;;;
;; No order Set  ;;
;;;;;;;;;;;;;;;;;;;
(setq foo '(a b c))                     ; => (a b c)
(remq 'b foo)                           ; => (a c)
foo                                     ; => (a b c)
;; delq might damage the parameter 
(delq 'b foo)                           ; => (a c)
foo                                     ; => (a c)
(delq 'a foo)                           ; => (c)
foo                                     ; => (a c)

;;;;;;;;;;;;;;;;;;;;;;;
;; association list  ;;
;;;;;;;;;;;;;;;;;;;;;;;
;;; assoc 用值做比较，assq 用地址做比较
(assoc "a" '(("a" 97) ("b" 98)))        ; => ("a" 97)
(assq "a" '(("a" 97) ("b" 98)))         ; => nil

(assq 'a '((a . 97) (b . 98)))          ; => (a . 97)
(assoc 'a '((a . 97) (b . 98)))          ; => (a . 97)

(cdr (assoc "a" '(("a" 97) ("b" 98))))  ; => (97)
(cdr (assq 'a '((a . 97) (b . 98))))    ; => 97

(assoc-default "a" '(("a" 97) ("b" 98)))          ; => (97)

(rassoc '(97) '(("a" 97) ("b" 98)))     ; => ("a" 97)
(rassq '97 '((a . 97) (b . 98)))        ; => (a . 97)

(setq foo '(("a" . 97) ("b" . 98)))     ; => (("a" . 97) ("b" . 98))

;; update value by setcdr
(if (setq bar (assoc "a" foo))
    (setcdr bar "this is a")
  (setq foo (cons '("a" . "this is a") foo))) ; => "this is a"
foo                         ; => (("a" . "this is a") ("b" . 98))

;; update value by delq and cons
(setq foo (cons '("a" . 97)
                (delq (assoc "a" foo) foo))) ; => (("a" . 97) ("b" . 98))
foo   ; => => (("a" . 97) ("b" . 98))

;;;;;;;;;;;
;; Tree  ;;
;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; Traverse  ;;
;;;;;;;;;;;;;;;
(mapc '1+ '(1 2 3))                     ; => (1 2 3)
(mapcar '1+ '(1 2 3))                   ; => (2 3 4)

(dolist (foo '(1 2 3))
  (incf foo))                           ; => nil

(setq bar nil)
(dolist (foo-tmp '(1 2 3) bar)
  (push (incf foo-tmp) bar))                ; => (4 3 2)
bar   ; => (4 3 2) 

;;;;;;;;;;;
;; Misc  ;;
;;;;;;;;;;;
(defun my-remove-if (predicate list)
  (delq nil (mapcar (lambda (n)
                      (and (not (funcall predicate n)) n))
                    list)))

(defun evenp (n)
  (= (% n 2) 0))

(my-remove-if 'evenp '(0 1 2 3 4 5))    ; => (1 3 5)

(defun my-fold-left (op initial list)
  (dolist (var list initial)
    (setq initial (funcall op initial var))))

(my-fold-left '+ 0 '(1 2 3 4))          ; => 10

(split-string "key = val" "\\s-*=\\s-*")  ; => ("key" "val")

(mapconcat 'identity '("a" "b" "c") "\t") ; => "a     b     c"

