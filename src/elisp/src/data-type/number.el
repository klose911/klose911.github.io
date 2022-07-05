;;;;;;;;;;;;
;; number ;;
;;;;;;;;;;;;


#b101100 ;; => 44 二进制
#o54 ;; => 44  八进制
#x2c ;; => 44  十进制

#24r1k ;; => 44        ; 二十四进制


;;; test 
(integerp 1.)                           ; => t
(integerp 1.0)                          ; => nil
(floatp 1.)                             ; => nil
(floatp -0.0e+NaN)                      ; => t
(numberp 1)                             ; => t

;;; compare
(setq foo (- (+ 1.0 1.0e-3) 1.0))       ; => 0.0009999999999998899
(setq bar 1.0e-3)                       ; => 0.001
(= foo bar)                             ; => nil

(defvar fuzz-factor 1.0e-6)

(defun approx-equal (x y)
  (or (and (= x 0) (= y 0))
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         fuzz-factor)))
(approx-equal foo bar)                  ; => t

;; also check number type 
(= 1.0 1)                               ; => t
(eql 1.0 1)                             ; => nil

;; (floor 1e20) ;; => (range-error "floor" 1e+20)

;;; calcualtion
(setq foo 10)                           ; => 10
(setq foo (1+ foo))                     ; => 11
(setq foo (1- foo))                     ; => 10

;; (+ (% DIVIDEND DIVISOR)
;;    (* (/ DIVIDEND DIVISOR) DIVISOR))

;; (+ (mod DIVIDEND DIVISOR)
;;    (* (floor DIVIDEND DIVISOR) DIVISOR))
