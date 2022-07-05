(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("notes-html"
         :base-directory "~/Documents/programming/html/klose911.github.io/src" 
         :base-extension "org"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/material/"
         :recursive nil
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

        ;; css
        ("notes-css"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/css" ;; Change this to your local dir
         :base-extension "css"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/material/css"
         :recursive t
         :publishing-function org-publish-attachment
         )

	;; picture
        ("notes-pic"
         :base-directory "~/Documents/programming/html/klose911.github.io/src/pic"  ;; Change this to your local dir
         :base-extension "png\\|jpg\\|gif"
         :publishing-directory "~/Documents/programming/html/klose911.github.io/html/material/pic"
         :recursive t
         :publishing-function org-publish-attachment
         )
	
        ("note" :components ("notes-css" "notes-pic" "notes-html"))
        )
      )

(defun note-publish (no-cache)
  "Publish note"
  (interactive "sno-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (org-publish "note" t)
    (org-publish "note" nil)))
