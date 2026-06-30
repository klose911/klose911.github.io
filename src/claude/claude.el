(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("claude-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/claude" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/claude"
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
        ("claude-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/claude/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	        ;; These are static files (images, pdf, etc)
        ("claude-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/claude/pic" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/claude/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("claude" :components ("claude-notes" "claude-static" "claude-pic"))
        )
      )

(defun claude-publish (no-cache)
  "Publish claude"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "claude" t)
    (org-publish "claude" nil)))
