;;;;;;;;;;;;;;;;;;;;
;; let expression ;;
;;;;;;;;;;;;;;;;;;;;

(let ((i 1) (j 2))
  (+ i j)) ; 3

(let ((i 1))
  (let ((j (+ i 2)))
    (* i j))) ; 3 

(let ((i 1)
      (j (+ i 2)))
  (* i j)) ;  i: undefined

;;; let*
(let* ((i 1)
       (j (+ i 2)))
  (* i j)) ;  3

;;;The scopes of variables d,e, and f are the regions with the same background colors.
(define (quadric-equation a b c)
  (if (zero? a)      
      'error                                      ; 1
      (let ((d (- (* b b) (* 4 a c))))            ; 2
        (if (negative? d)
            '()                                      ; 3
            (let ((e (/ b a -2)))                    ; 4
              (if (zero? d)
              (list e)
              (let ((f (/ (sqrt d) a 2)))        ; 5
                (list (+ e f) (- e f)))))))))

(quadric-equation 3 5 2)  ; (-2/3 -1) 
