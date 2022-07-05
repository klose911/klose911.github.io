;;;;;;;;;;;;
;; Buffer ;;
;;;;;;;;;;;;

;;; name 
(buffer-name) ;; => "buffer.el"

;;; current buffer 
(set-buffer "*Messages*")   ; => #<buffer *Messages*>
(message (buffer-name))                  ; => "*scratch*"
(progn
  (set-buffer "*Messages*")
  (message (buffer-name)))               ; "*Messages*"

;; (let (buffer-read-only
;;       (obuf (current-buffer)))
;;   (set-buffer ...)
;;   ...
;;   (set-buffer obuf))

(save-current-buffer
  (set-buffer "*scratch*")
  (goto-char (point-min))
  (set-buffer "*Messages*"))

(save-excursion
  (set-buffer "*scratch*")
  (goto-char (point-min))
  (set-buffer "*Messages*"))

;;; postion and marker
(setq foo (make-marker))             ; => #<marker in no buffer>
(set-marker foo (point))             ; => #<marker at 740 in buffer.el>

(point-marker)                       ; => #<marker at 802 in buffer.el>
(copy-marker 20)                     ; => #<marker at 20 in buffer.el>
(copy-marker foo)                    ; => #<marker at 740 in buffer.el>

(marker-position foo)                ; => 740
(marker-buffer foo)                  ; => #<buffer buffer.el>

;;; move 
(goto-char (point-min))                   ; 跳到缓冲区开始位置
(forward-char 10)                         ; 向前移动 10 个字符
(forward-char -10)                        ; 向后移动 10 个字符

