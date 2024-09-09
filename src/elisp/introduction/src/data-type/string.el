;;;;;;;;;;;;
;; String ;;
;;;;;;;;;;;;

(setq foo "abc\000abc")                 ; => "abc^@abc"

;;; alphabet 
?A                                      ; => 65
?a                                      ; => 97

;;; punction char 
?\a ;; => 7                 ; control-g, `C-g'
?\b ;; => 8                 ; backspace, <BS>, `C-h'
?\t ;; => 9                 ; tab, <TAB>, `C-i'
?\n ;; => 10                ; newline, `C-j'
?\v ;; => 11                ; vertical tab, `C-k'
?\f ;; => 12                ; formfeed character, `C-l'
?\r ;; => 13                ; carriage return, <RET>, `C-m'
?\e ;; => 27                ; escape character, <ESC>, `C-['
?\s ;; => 32                ; space character, <SPC>
?\\ ;; => 92                ; backslash character, `\'
?\d ;; => 127               ; delete character, <DEL>

;;; control char
?\^I ;; => 9 (#o11, #x9, ?\C-i)
?\^i ;; => 9 (#o11, #x9, ?\C-i)
?\C-I ;; => 9 (#o11, #x9, ?\C-i)
?\C-i ;; => 9 (#o11, #x9, ?\C-i)

;;; meta char
(logior (lsh 1 27) ?A)                  ; => 134217793
?\M-A                                   ; => 134217793

;;; type function
(defun string-emptyp (str)
  (not (string< "" str)))

(string-emptyp "")

;;; constructor
(make-string 5 ?x)                      ; => "xxxxx"
(string ?a ?b ?c)                       ; => "abc"

(substring "0123456789" 3)              ; => "3456789"
(substring "0123456789" 3 5)            ; => "34"
(substring "0123456789" -3 -1)          ; => "78"

;;; compare string

;;; convert
(string-to-number "256")                ; => 256
(number-to-string 256)                  ; => "256"
(format "%#o" 256)                      ; => "0400"
(format "%#x" 256)                      ; => "0x100"

(defun number-to-bin-string (number)
  (require 'calculator)
  (let ((calculator-output-radix 'bin)
        (calculator-radix-grouping-mode nil))
    (calculator-number-to-string number)))

(number-to-bin-string 256)              ; => "100000000"

(concat '(?a ?b ?c ?d ?e))              ; => "abcde"
(concat [?a ?b ?c ?d ?e])               ; => "abcde"
(vconcat "abdef")                       ; => [97 98 100 101 102]
(append "abcdef" nil)                   ; => (97 98 99 100 101 102)

(downcase "The cat in the hat")         ; => "the cat in the hat"
(downcase ?X)                           ; => 120
(upcase "The cat in the hat")           ; => "THE CAT IN THE HAT"
(upcase ?x)                             ; => 88
(capitalize "The CAT in tHe hat")       ; => "The Cat In The Hat"
(upcase-initials "The CAT in the hAt")  ; => "The CAT In The HAt"
;;; format

;;; search
(string-match "34" "01234567890123456789")    ; => 3
(string-match "34" "01234567890123456789" 10) ; => 13

(string-match "2*" "232*3=696")                ; => 0
(string-match (regexp-quote "2*") "232*3=696") ; => 2

(progn
  (string-match "3\\(4\\)" "01234567890123456789")
  (match-data))                         ; => (3 5 4 5)

(let ((start 0))
  (while (string-match "34" "01234567890123456789" start)
    (princ (format "find at %d\n" (match-beginning 0)))
    (setq start (match-end 0))))

;; find at 3
;; find at 13
;; nil


(let ((str "01234567890123456789"))
  (string-match "34" str)
  (princ (replace-match "x" nil nil str 0))
  (princ "\n")
  (princ str))

;; 012x567890123456789
;; 01234567890123456789"01234567890123456789" 
