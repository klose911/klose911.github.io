(+ 1 2) ; => 3

(+)       ;?? 0
(+ 1)     ;?? 1
(+ 1 2)   ;?? 3
(+ 1 2 3) ;?? 6

(- 10 3)    ;?? 7
(- 10 3 5)  ;?? 2
(* 2 3)     ;?? 6
(* 2 3 4)   ;?? 24
(/ 29 3)    ;?? 29/3
(/ 29 3 7)  ;?? 29/21
(/ 9 6)     ;?? 3/2
(exact->inexact (/ 29 3 7)) ;?? 1.380952380952381

(* (+ 2 3) (- 5 3)) ;?? 10
(/ (+ 9 1) (+ 2 3)) ;?? 2

(quotient 7 3) ;?? 2
(modulo 7 3)   ;?? 1
(sqrt 8)       ;?? 2.8284271247461903

(atan 1)   ;?? 0.7853981633974483
(atan 1 0) ;?? 1.5707963267948966
