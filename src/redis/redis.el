(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("redis-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/redis" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/redis"
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
        ("redis-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/redis/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("redis-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/redis/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/redis/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("redis" :components ("redis-notes" "redis-static" "redis-pic"))
        )
      )

(defun redis-publish (no-cache)
  "Publish redis"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "redis" t)
    (org-publish "redis" nil)))
