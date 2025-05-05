(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("nio-notes"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/nio" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/nio"
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
        ("nio-static"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/nio/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; These are image files (images, pdf, etc)
        ("nio-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/nio/pic" ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif\\|mp4\\|webp"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/nio/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("nio" :components ("nio-notes" "nio-static" "nio-pic"))
        )
      )

(defun nio-publish (no-cache)
  "Publish nio"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "nio" t)
    (org-publish "nio" nil)))
