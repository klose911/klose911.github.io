;;;;;;;;;;;;;;;
;; Variable  ;;
;;;;;;;;;;;;;;;

;;; local binding
(progn
  (setq foo "I'm global variable!")
  (let ((foo 5))
    (message "foo value is: %S" foo) ; => foo value is: 5
    (let (foo)
      (setq foo "I'm local variable!") 
      (message foo)) ; => I’m local variable!
    (message "foo value is still: %S" foo)) ; => foo value is still: 5 
  (message foo)) ; => I’m global variable!
;; => "I’m global variable!"

;;; buffer local variable
(setq foo "I'm global variable!")       ; => "I'm global variable!"
(make-local-variable 'foo)              ; => foo
foo                                     ; => "I'm global variable!"
(setq foo "I'm buffer-local variable!") ; => "I'm buffer-local variable!"
foo                                  ; => "I'm buffer-local variable!"
(with-current-buffer "*Messages*" foo)  ; => "I'm global variable!"

(default-value 'foo)                    ; => "I'm global variable!"


(local-variable-p 'foo)                           ; => t
(local-variable-p 'foo (get-buffer "*Messages*")) ; => nil

(with-current-buffer "*Messages*"
  (buffer-local-value 'foo (get-buffer "*scratch*"))) ; => "I'm buffer local variable!"

(defun binder (x)                      ; `x' is bound in `binder'.
  (foo 5))                             ; `foo' is some other function.

(defun user ()                         ; `x' is used "free" in `user'.
  (list x))

(defun foo (ignore)
  (user))

(binder 10)                            ; => (10)

(defun make-add (n)
  (function (lambda (m) (+ n m))))      ; Return a function.

(fset 'add2 (make-add 2))               ; Define function `add2'  with `(make-add 2)'.

; Try to add 2 to 4. 
(add2 4)  ;; failed: add2: Symbol’s value as variable is void: n

;;; misc function 
foo                                     ; => "I'm local variable!"
(boundp 'foo)                           ; => t
(default-boundp 'foo)                   ; => t
(makunbound 'foo)                       ; => foo
foo                                     ; This will signal an error
(default-boundp 'foo)                   ; => t
(kill-local-variable 'foo)              ; => foo
