(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("sqlite-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/sqlite" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/sqlite"
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
        ("sqlite-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/sqlite/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("sqlite-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/sqlite/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/sqlite/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("sqlite" :components ("sqlite-notes" "sqlite-static" "sqlite-pic"))
        )
      )

(defun sqlite-publish (no-cache)
  "Publish sqlite"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "sqlite" t)
    (org-publish "sqlite" nil)))
