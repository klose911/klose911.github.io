;;;;;;;;;;;;;;;
;; sequence  ;;
;;;;;;;;;;;;;;;

(safe-length '(a . b))                  ; => 1
(safe-length '#1=(1 2 . #1#))           ; => 3

;;; type error 
;; (length '(a . b))

;;; dangerous 
;; (length '#1=(1 2 . #1#))

;;;;;;;;;;;;
;; Vector ;;
;;;;;;;;;;;;
(vector 'foo 23 [bar baz] "rats")       ; => [foo 23 [bar baz] "rats"]


(setq foo '(a b)) 
foo                                     ; => (a b)
[foo]                                   ; => [foo]
(vector foo)                            ; => [(a b)]

(make-vector 9 'Z)                      ; => [Z Z Z Z Z Z Z Z Z]

(fillarray (make-vector 3 'Z) 5)        ; => [5 5 5]

(vconcat [A B C] "aa" '(foo (6 7)))     ; => [A B C 97 97 foo (6 7)]

;;;;;;;;;;;;;;;;;;;
;; Applications  ;;
;;;;;;;;;;;;;;;;;;;
(defun circular-list-p (list)
  (and (consp list)
       (circular-list-p-1 (cdr list) list 0)))

(defun circular-list-p-1 (tail halftail len)
  (if (eq tail halftail)
      t
    (if (consp tail)
        (circular-list-p-1 (cdr tail)
                           (if (= (% len 2) 0)
                               (cdr halftail)
                             halftail)
                           (1+ len))
      nil)))

(defun my-tr (str from to)
  (if (= (length to) 0)                 ; 空字符串
      (progn
        (setq from (append from nil))
        (concat
         (delq nil
               (mapcar (lambda (c)
                         (if (member c from)
                             nil c))
                       (append str nil)))))
    (let (table newstr pair)
      ;; 构建转换表
      (dotimes (i (length from))
        (push (cons (aref from i) (aref to i)) table))
      (dotimes (i (length str))
        (push
         (if (setq pair (assoc (aref str i) table))
             (cdr pair)
           (aref str i))
         newstr))
      (concat (nreverse newstr) nil))))


(my-tr "abc abc cde" [?a ?b] [?b ?f])  
