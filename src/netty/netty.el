(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("netty-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/netty" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/netty"
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
        ("netty-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/netty/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; These are image files (images, pdf, etc)
        ("netty-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/netty/pic" ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/netty/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("netty" :components ("netty-notes" "netty-static" "netty-pic"))
        )
      )

(defun netty-publish (no-cache)
  "Publish netty"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "netty" t)
    (org-publish "netty" nil)))
