
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define simple functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hello world as a variable
(define vhello "Hello world")    

;; Hello world as a function
(define fhello (lambda ()         
		 "Hello world"))


vhello ; "Hello world"

fhello ; #<procedure:fhello>

(fhello) ; "Hello world"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define function with parameters ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hello with name
(define hello
  (lambda (name)
    (string-append "Hello " name "!")))

;; sum of three numbers
(define sum3
  (lambda (a b c)
    (+ a b c)))

(hello "Lucy") ; "Hello Lucy!"

(sum3 10 20 30) ; 60

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define function with short term ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hello with name
(define (hello name)
  (string-append "Hello " name "!"))


;; sum of three numbers
(define (sum3 a b c)
  (+ a b c))
