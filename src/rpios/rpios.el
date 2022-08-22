(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("rpios-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/rpios" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/rpios"
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
        ("rpios-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/rpios/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; These are static files (images, pdf, etc)
        ("rpios-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/rpios/pic" ;; Change this to your local dir
         :base-extension "svg\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/rpios/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("rpios" :components ("rpios-notes" "rpios-static" "rpios-pic"))
        )
      )

(defun rpios-publish (no-cache)
  "Publish rpios"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "rpios" t)
    (org-publish "rpios" nil)))
