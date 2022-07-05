;;;;;;;;;;;;;;;;;;;;;;;
;; Assoscation Lists ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define wc '((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 8)))
(assq 'hi wc) ; => (hi .3) 
(assq 'you wc) ; => (you . 8) 
(assq 'i wc) ; => #f 

(define n '((1 2 3) (4 5 6) (7 8 9)))
(assv 1 n) ; => ( 1 2 3) 
(assv 8 n) ; => #f 

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mit-scheme hashtables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
