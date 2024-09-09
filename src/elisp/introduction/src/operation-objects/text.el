;;;;;;;;;;
;; Text ;;
;;;;;;;;;;

;;; property
(propertize "abc" 'face 'bold)          ; => #("abc" 0 3 (face bold))

;; get property 
(setq foo (concat "abc"
                  (propertize "cde" 'face 'bold))) ; => #("abccde" 3 6 (face bold))
(get-text-property 3 'face foo)                    ; => bold

(save-excursion
  (goto-char (point-min))
  (insert foo))

(get-text-property 4 'face)                        ; => bold

;; modify 
(let ((str "abc"))
  (put-text-property 0 3 'face 'bold str)
  str)                                  ; => #("abc" 0 3 (face bold))

(setq foo (propertize "abcdef" 'face 'bold
                      'pointer 'hand)) ;; => #("abcdef" 0 6 (pointer hand face bold))
(set-text-properties 0 2 nil foo)       ; => t
foo   ; => #("abcdef" 2 6 (pointer hand face bold))
(remove-text-properties 2 4 '(face nil) foo) ; => t
foo   ; => #("abcdef" 2 4 (pointer hand) 4 6 (pointer hand face bold))
(remove-list-of-text-properties 4 6 '(face nil pointer nil) foo) ; => t
foo   ; => #("abcdef" 2 4 (pointer hand))

;;; search
(setq foo (concat "ab"
                  (propertize "cd" 'face 'bold)
                  (propertize "ef" 'pointer 'hand))) ;; => #("abcdef" 2 4 (face bold) 4 6 (pointer hand))
(next-property-change 1 foo)                  ; => 2
(next-single-property-change 1 'pointer foo)  ; => 4
(previous-property-change 6 foo)              ; => 4
(previous-single-property-change 6 'face foo) ; => 4

(text-property-any 0 6 'face 'bold foo)          ; => 2
(text-property-any 0 6 'face 'underline foo)     ; => nil
(text-property-not-all 2 6 'face 'bold foo)      ; => 4
(text-property-not-all 2 6 'face 'underline foo) ; => 2
