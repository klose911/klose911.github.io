;;;;;;;;;;;
;; File  ;;
;;;;;;;;;;;

;;; read 
(find-file "~/tmp/test.txt")
(with-current-buffer
    (find-file-noselect "~/tmp/test.txt")
  buffer-file-name)                     ; => "/home/klose/tmp/test.txt"
(find-buffer-visiting "~/tmp/test.txt") ; => #<buffer test.txt>
(get-file-buffer "~/tmp/test.txt")      ; => #<buffer test.txt>

;;; write

;;; info 
(file-exists-p "~/tmp/test.txt")              ; => t
(file-readable-p "~/tmp/test.txt")            ; => t
(file-writable-p "~/tmp/test.txt")            ; => t
(file-executable-p "~/tmp/test.txt")          ; => nil
(format "%o" (file-modes "~/tmp/test.txt"))   ; => "644"

(file-regular-p "~/tmp/t.txt")         ; => t
(file-directory-p "~/tmp/t.txt")       ; => nil
(file-symlink-p "~/tmp/t.txt")         ; => "test.txt"
(file-truename "~/tmp/t.txt")          ; => "/home/klose/tmp/test.txt"

(defun file-stat-type (file &optional id-format)
  (car (file-attributes file id-format)))
(defun file-stat-name-number (file &optional id-format)
  (cadr (file-attributes file id-format)))
(defun file-stat-uid (file &optional id-format)
  (nth 2 (file-attributes file id-format)))
(defun file-stat-gid (file &optional id-format)
  (nth 3 (file-attributes file id-format)))
(defun file-stat-atime (file &optional id-format)
  (nth 4 (file-attributes file id-format)))
(defun file-stat-mtime (file &optional id-format)
  (nth 5 (file-attributes file id-format)))
(defun file-stat-ctime (file &optional id-format)
  (nth 6 (file-attributes file id-format)))
(defun file-stat-size (file &optional id-format)
  (nth 7 (file-attributes file id-format)))
(defun file-stat-modes (file &optional id-format)
  (nth 8 (file-attributes file id-format)))
(defun file-stat-guid-changep (file &optional id-format)
  (nth 9 (file-attributes file id-format)))
(defun file-stat-inode-number (file &optional id-format)
  (nth 10 (file-attributes file id-format)))
(defun file-stat-system-number (file &optional id-format)
  (nth 11 (file-attributes file id-format)))
(defun file-attr-type (attr)
  (car attr))
(defun file-attr-name-number (attr)
  (cadr attr))
(defun file-attr-uid (attr)
  (nth 2 attr))
(defun file-attr-gid (attr)
  (nth 3 attr))
(defun file-attr-atime (attr)
  (nth 4 attr))
(defun file-attr-mtime (attr)
  (nth 5 attr))
(defun file-attr-ctime (attr)
  (nth 6 attr))
(defun file-attr-size (attr)
  (nth 7 attr))
(defun file-attr-modes (attr)
  (nth 8 attr))
(defun file-attr-guid-changep (attr)
  (nth 9 attr))
(defun file-attr-inode-number (attr)
  (nth 10 attr))
(defun file-attr-system-number (attr)
  (nth 11 attr))

;;; file name
(file-name-directory "~/tmp/test.txt")      ; => "~/tmp/"
(file-name-nondirectory "~/tmp/test.txt")   ; => "test.txt"
(file-name-sans-extension "~/tmp/test.txt") ; => "~/tmp/test"
(file-name-extension "~/tmp/test.txt")      ; => "txt"
(file-name-sans-versions "~/tmp/test.txt~") ; => "~/tmp/test.txt"
(file-name-sans-versions "~/tmp/test.txt.~1~") ; => "~/tmp/test.txt"

(file-name-absolute-p "~rms/foo")       ; => t
(file-name-absolute-p "/user/rms/foo")  ; => t
(expand-file-name "foo")                ; => "/home/klose/foo"
(expand-file-name "foo" "/usr/spool/")  ; => "/usr/spool/foo"
(file-relative-name "/foo/bar" "/foo/") ; => "bar"
(file-relative-name "/foo/bar" "/hack/") ; => "../foo/bar"

(file-name-as-directory "~rms/lewis")   ; => "~rms/lewis/"
(directory-file-name "~lewis/")         ; => "~lewis"

(convert-standard-filename "c:/windows")  ;=> "c:\\windows"

;;; temp file and dir
(make-temp-file "foo")                  ; => "/tmp/foo5611dxf"
(make-temp-name "foo")                  ; => "foo5611q7l"

;;; traverse dir
(directory-files "~/tmp/")
;; =>
;; ("#foo.el#" "." ".#foo.el" ".." "foo.el" "t.pl" "t2.pl")
(directory-files "~/tmp" t)
;; =>
;; ("/home/ywb/tmp/dir/#foo.el#"
;;  "/home/ywb/tmp/dir/."
;;  "/home/ywb/tmp/dir/.#foo.el"
;;  "/home/ywb/tmp/dir/.."
;;  "/home/ywb/tmp/dir/foo.el"
;;  "/home/ywb/tmp/dir/t.pl"
;;  "/home/ywb/tmp/dir/t2.pl")
(directory-files "~/tmp/" nil "\\.txt$") ; => ("t.pl" "t2.pl")

;;; file handle
(defun my-scratch-auto-save-file-name (operation &rest args)
  (if (and (eq operation 'expand-file-name)
           (string= (car args) "#*scratch*#"))
      (expand-file-name (concat "~/.emacs.d/backup/" (car args)))
    (let ((inhibit-file-name-handlers
           (cons 'my-scratch-auto-save-file-name
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args))))
