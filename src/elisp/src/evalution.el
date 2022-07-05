;;;;;;;;;;;;;;;;;;;;;;
;; Value Evalution  ;;
;;;;;;;;;;;;;;;;;;;;;;
(symbol-function 'car)                  ; => #<subr car>
(fset 'first 'car)                      ; => car
(fset 'erste 'first)                    ; => first
(erste '(1 2 3))                        ; => 1

(defmacro my-cadr (x)
  (list 'car (list 'cdr x)))
