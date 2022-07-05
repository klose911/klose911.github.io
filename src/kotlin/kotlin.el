(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("kotlin-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/kotlin" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/kotlin"
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
        ("kotlin-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/kotlin/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; These are static files (images, pdf, etc)
        ("kotlin-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/kotlin/pic" ;; Change this to your local dir
         :base-extension "svg\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/kotlin/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("kotlin" :components ("kotlin-notes" "kotlin-static" "kotlin-pic"))
        )
      )

(defun kotlin-publish (no-cache)
  "Publish kotlin"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "kotlin" t)
    (org-publish "kotlin" nil)))
