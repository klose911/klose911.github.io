(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("redis-code-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/redis/code" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/redis/code"
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
        ("redis-code-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/redis/code/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("redis-code-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/redis/code/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/redis/code/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("redis-code" :components ("redis-code-notes" "redis-code-static" "redis-code-pic"))
        )
      )

(defun redis-code-publish (no-cache)
  "Publish redis-code"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "redis-code" t)
    (org-publish "redis-code" nil)))
