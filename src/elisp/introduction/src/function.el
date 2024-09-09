;;;;;;;;;;;;;;;;;;;;;;
;; Function  ;;
;;;;;;;;;;;;;;;;;;;;;;
;;; optional and rest parameters 

(defun foo (var1 var2 &optional opt1 opt2 &rest rest)
  (list var1 var2 opt1 opt2 rest))

(foo 1 2)                               ; => (1 2 nil nil nil)
(foo 1 2 3)                             ; => (1 2 3 nil nil)
(foo 1 2 3 4 5 6)                       ; => (1 2 3 4 (5 6))

;;; function docs
(defun foo (var1 var2 &optional opt1 opt2 &rest rest)
  "You should call the function like:

\(fn v1 v2)"
  (list var1 var2 opt1 opt2 rest))

(defun foo ()
  "A simple document string to show how to use `' and \\=\\{}.
You can press this button `help' to see the document of
function \"help\".

This is keybind of text-mode(substitute from \\=\\{text-mode-map}):
\\{text-mode-map}

See also `substitute-command-keys' and `documentation'"
  )

;;; call function
(funcall 'list 'x '(y) '(z a))               ; => (x (y) (z a))
(apply 'list 'x '(y ) '(z a))                ; => (x (y) z a)

;;;;;;;;;;;
;; Macro ;;
;;;;;;;;;;;
(defmacro foo (arg)
  (list 'message "%d %d" arg arg))

(defun bar (arg)
  (message "%d %d" arg arg))

(let ((i 1))
  (bar (incf i)))                       ; => "2 2"

(let ((i 1))
  (foo (incf i)))                       ; => "2 3"

(macroexpand '(foo (incf i))) ; => (message "%d %d" (incf i) (incf i))

;;; macro declare
(defmacro my-when (cond &rest body)
  (declare (indent 1) (debug t))
  (list 'if cond (cons 'progn body)))

(symbol-plist 'my-when)    ; => (lisp-indent-function 1 edebug-form-spec t) 

`(a list of ,(+ 2 3) elements)          ; => (a list of 5 elements)
(setq some-list '(2 3))                 ; => (2 3)
`(1 ,some-list 4 ,@some-list)           ; => (1 (2 3) 4 2 3)

(defmacro my-when (cond &rest body)
  `(if ,cond
       (progn ,@body)))

;;;;;;;;;;;;;
;; Command ;;
;;;;;;;;;;;;;
(defun hello-world (name)
  (interactive "sWhat you name? ")
  (message "Hello, %s" name))

(defun hello-world (name time)
  (interactive "sWhat you name? \nnWhat the time? ")
  (message "Good %s, %s"
           (cond ((< time 13) "morning")
                 ((< time 19) "afternoon")
                 (t "evening"))
           name))

(read-string "What your name? " user-full-name)

(defun foo ()
  (interactive)
  (message "%S" current-prefix-arg))

(defun read-hiden-file (file arg)
  (interactive
   (list (read-file-name "Choose a hiden file: " "~/" nil nil nil
                         (lambda (name)
                           (string-match "^\\." (file-name-nondirectory name))))
         current-prefix-arg))
  (message "%s, %S" file arg))
