(eq? (string->symbol "Hello") 'Hello) ; #f 

(eq? (string->symbol "Hello") (string->symbol "Hello")) ; #t 

(symbol->string  (string->symbol "Hello")) ; "Hello"
