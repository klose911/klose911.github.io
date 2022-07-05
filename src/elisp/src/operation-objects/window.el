;;;;;;;;;;;;;;;;;;;;
;; Window & Frame ;;
;;;;;;;;;;;;;;;;;;;;
;;; split 
(split-window &optional window size horizontal)

(selected-window)                       ; => #<window 136 on *scratch*>
(split-window)                          ; => #<window 138 on *scratch*>

;;; close 
(setq foo (selected-window))            ; => #<window 90 on *scratch*>
(delete-window)
(windowp foo)                           ; => t
(window-live-p foo)                     ; => nil

;;; configuration 
(setq foo (current-window-configuration))
;; do sth to make some changes on windows
(set-window-configuration foo)

;; height and width
(window-height)                         ; => 45
(window-body-height)                    ; => 44

(window-width)                          ; => 72

(window-edges)                          ; => (0 0 73 45)

(window-inside-edges)                   ; => (1 0 73 44)
(window-pixel-edges)                    ; => (0 0 511 675)
(window-inside-pixel-edges)             ; => (7 0 511 660)

;; buffer
(window-buffer)                         ; => #<buffer *scratch*>
(window-buffer (next-window))           ; => #<buffer *info*>

(get-buffer-window (get-buffer "*scratch*"))
;; => #<window 268 on *scratch*>
(get-buffer-window-list (get-buffer "*scratch*"))
;; => (#<window 268 on *scratch*> #<window 270 on *scratch*>)

;;; select
(selected-window)                       ; => #<window 104 on *scratch*>

(progn
  (setq foo (selected-window))
  (message "Original window: %S" foo)
  (other-window 1)
  (message "Current window: %S" (selected-window))
  (select-window foo)
  (message "Back to original window: %S" foo))

;; 让另一个窗口滚动到缓冲区开始
(save-selected-window
  (select-window (next-window))
  (goto-char (point-min)))


(selected-window)                       ; => #<window 245 on *scratch*>
(window-list)
;; => (#<window 245 on *scratch*> #<window 253 on *scratch*> #<window 251 on *info*>)
(next-window)                           ; => #<window 253 on *scratch*>
(next-window (next-window))             ; => #<window 251 on *info*>
(next-window (next-window (next-window)));  => #<window 245 on *scratch*>

