;;;;;;;;;;;;
;; Symbol ;;
;;;;;;;;;;;;

;;; naming rule 
(symbolp '+1)                           ; => nil
(symbolp '\+1)                          ; => t
(symbol-name '\+1)                      ; => "+1"

;; foo                 ; 名为 `foo' 的符号
;; FOO                 ; 名为 `FOO' 的符号，和 `foo' 不同
;; char-to-string      ; 名为 `char-to-string' 的符号
;; 1+                  ; 名为 `1+' 的符号 (不是整数 `+1')
;; \+1                 ; 名为 `+1' 的符号 (可读性很差的名字)
;; \(*\ 1\ 2\)         ; 名为 `(* 1 2)' 的符号 (更差劲的名字).
;; +-*/_~!@$%^&=:<>{}  ; 名为 `+-*/_~!@$%^&=:<>{}' 的符号.
;;                     ;   这些字符无须转义

;;; generate symbol
(setq foo (make-vector 10 0))           ; => [0 0 0 0 0 0 0 0 0 0]
(intern-soft "abc" foo)                 ; => nil
(intern "abc" foo)                      ; => abc
foo                                     ; =>  [0 0 0 0 0 0 0 0 0 foo]
(intern-soft "abc" foo)                 ; => abc

(intern-soft "abc")                     ; => nil
;; abc was added into global obarray
'abc                                    ; => abc 
(intern-soft "abc")                     ; => abc 
(intern-soft "abcd")                    ; => nil
;; abcd will not be added into global obarray 
'#:abcd                                 ; => abcd
(intern-soft "abcd")                    ; => nil

(intern-soft "abc" foo)                 ; => abc
(unintern "abc" foo)                    ; => t
(intern-soft "abc" foo)                 ; => nil

(setq count 0)                          ; => 0
(defun count-syms (s)
  (setq count (1+ count)))              ; => count-syms

(mapatoms 'count-syms)                  ; => nil
count                                   ; => 70395
(length obarray)                        ; => 15121

;;; hash-bucket
(defun hash-string (str)
  (let ((hash 0) c)
    (dotimes (i (length str))
      (setq c (aref str i))
      (if (> c #o140)
	  (setq c (- c 40)))
      (setq hash (+ (setq hash (lsh hash 3))
		    (lsh hash -28)
		    c)))
    hash))

(let ((len 10) str hash)
  (setq foo (make-vector len 0))
  (dotimes (i (1+ len))
    (setq str (char-to-string (+ ?a i))
	  hash (% (hash-string str) len))
    (message "I put %s in slot %d"
	     str hash)
    (if (eq (aref foo hash) 0)
	(intern str foo)
      (message "I found %S is already taking the slot: %S"
	       (aref foo hash) foo)
      (intern str foo)
      (message "Now I'am in the slot too: %S" foo))))

;; I put a in slot 7
;; I put b in slot 8
;; I put c in slot 9
;; I put d in slot 0
;; I put e in slot 1
;; I put f in slot 2
;; I put g in slot 3
;; I put h in slot 4
;; I put i in slot 5
;; I put j in slot 6
;; I put k in slot 7
;; I found a is already taking the slot: [d e f g h i j a b c]
;; Now I’am in the slot too: [d e f g h i j k b c]

;;;;;;;;;;;;;;;;;;;;
;; Part of Symbol ;;
;;;;;;;;;;;;;;;;;;;;
;; value 
(set (intern "abc" foo) "I'm abc")      ; => "I'm abc"
(symbol-value (intern "abc" foo))       ; => "I'm abc"

;; function 
(fset (intern "abc" foo) (symbol-function 'car)) ; => #<subr car>
(funcall (intern "abc" foo) '(a . b))            ; => a

;; property list
(put (intern "abc" foo) 'doc "this is abc")      ; => "this is abc"
(get (intern "abc" foo) 'doc)                    ; => "this is abc"
(symbol-plist (intern "abc" foo))                ; => (doc "this is abc")

(plist-get '(foo 4) 'foo)               ; => 4
(plist-get '(foo 4 bad) 'bar)           ; => nil
(setq my-plist '(bar t foo 4))          ; => (bar t foo 4)
(setq my-plist (plist-put my-plist 'foo 69)) ; => (bar t foo 69)
(setq my-plist (plist-put my-plist 'quux '(a))) ; => (bar t foo 69 quux (a))

(defun my-plist-get (plist prop)
  (cadr (memq plist prop)))

(defun my-plist-put (plist prop val)
  (let ((tail (memq prop plist)))
    (if tail
        (setcar (cdr tail) val)
      (setcdr (last plist) (list prop val))))
  plist)
