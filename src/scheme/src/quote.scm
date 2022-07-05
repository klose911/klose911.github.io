(define a 1)
(define b 2)

'(a b)                          ; => (a b) 
`(a b)                          ; => '(a b)

`(a ,b)                         ; => '(a 2), a 依旧是那个变量 a
(quasiquote (a (unquote b)))    ; => '(a 2), 和 `(a ,b) 等价

`(,a ,b)                        ; => '(1 2)

(define c '(3 4 5))
`(,b ,@c)                       ; => (2 3 4 5)  
