(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("lisp-manual-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/lisp/manual" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/lisp/manual"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap nil
	 :section-numbers nil
         ;; :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                       href=\"css/main.css\" type=\"text/css\"/>"
         :html-preamble t
	 :html-postamble klose-html-postamble
	 :htmlized-source t
         )

        ;; These are static files (images, pdf, etc)
        ("lisp-manual-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/lisp/manual/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("lisp-manual-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/lisp/manual/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/lisp/manual/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("lisp-manual" :components ("lisp-manual-notes" "lisp-manual-static" "lisp-manual-pic"))
        )
      )

(defun lisp-manual-publish (no-cache)
  "Publish elisp"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "lisp-manual" t)
    (org-publish "lisp-manual" nil)))
